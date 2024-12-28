-- File Name   : Bayer_Demosaic_Registers.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Module implementing register file logic for the Bayer_Demosaic
--               IP core.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

-- Module packages
library work;
use work.Bayer_Demosaic_Package.all;
use work.CL_Video_Defs.all;
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Registers is
  generic
  (
    -- PEL_DEPTH             - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK      - Number of horizontal pixel values per clock
    -- SUPPORTS_BACKPRESSURE - Backpressure support flag
    PEL_DEPTH             : positive;
    PIXELS_PER_CLOCK      : positive;
    SUPPORTS_BACKPRESSURE : boolean
  );
  port
  (
    -- Host reset and clock 
    Host_Reset : in std_logic;
    Host_Clock : in std_logic;
   
    -- Host control interface
    Host_Enable     : in  std_logic;
    Host_Address    : in  Bayer_Host_Address;
    Host_Write      : in  std_logic;
    Host_WriteData  : in  std_logic_vector(31 downto 0);
    Host_ByteEnable : in  std_logic_vector(3 downto 0);
    Host_ReadData   : out std_logic_vector(31 downto 0);
    Host_Ack        : out std_logic;
    Host_Interrupt  : out std_logic;

    -- Global register file outputs
    Core_Enable : out std_logic;

    -- Interface to input stream parser
    Frame_Header_Strobe : in std_logic;
    Frame_Resolution    : in CL_Video_Coordinate;
    Frame_Roi_Offset    : in CL_Video_Coordinate;
    Frame_Index         : in CL_Video_Frame_Index;
    Frame_Timestamp     : in CL_Video_Frame_Timestamp;

    -- Interface to demosaic pipeline
    Row_Mode            : out Row_Mode_Type;
    Column_Mode         : out Column_Mode_Type;
    White_Balance_Gains : out White_Balance_Gain_Array((NUM_RGB_PLANES - 1) downto 0)
  );
end entity;


-- Architecture definition
architecture rtl of Bayer_Demosaic_Registers is

  -- Register file signals
  signal host_Enable_d    : std_logic;
  signal access_Pending   : std_logic;
  signal core_Enable_Reg  : std_logic;
  signal capture_Header   : std_logic;
  signal capture_Complete : std_logic;
  signal column_Mode_Reg  : Column_Mode_Type;
  signal row_Mode_Reg     : Row_Mode_Type;
  signal irq_Flags_Reg    : std_logic_vector((NUM_IRQS - 1) downto 0);
  signal irq_Mask_Reg     : std_logic_vector((NUM_IRQS - 1) downto 0);

  -- Video header capture registers
  signal header_Frame_Res_Reg       : CL_Video_Coordinate;
  signal header_Frame_Roi_Reg       : CL_Video_Coordinate;
  signal header_Frame_Index_Reg     : CL_Video_Frame_Index;
  signal header_Frame_Timestamp_Reg : CL_Video_Frame_Timestamp;

  -- Global white balance gain registers
  signal white_Balance_Gain_Regs : White_Balance_Gain_Array((NUM_RGB_PLANES - 1) downto 0);

begin

  -- Process handling host accesses
  host_Regs : process(Host_Reset, Host_Clock)
    variable field_Width   : positive;
    variable field_High    : natural;
    variable field_Low     : natural;
    variable new_Irq_Flags : std_logic_vector((NUM_IRQS - 1) downto 0);
    variable new_Irq_Mask  : std_logic_vector((NUM_IRQS - 1) downto 0);
  begin
    if(Host_Reset = '1') then
      access_Pending          <= '0';
      core_Enable_Reg         <= '0';
      column_Mode_Reg         <= GREEN_FIRST;
      row_Mode_Reg            <= RED_FIRST;
      capture_Header          <= '0';
      irq_Flags_Reg           <= (others => '0');
      irq_Mask_Reg            <= (others => '0');
      white_Balance_Gain_Regs <= (others => WHITE_BALANCE_UNITY_GAIN);
      Host_Ack                <= '0';
      Host_ReadData           <= (others => '0');
      Host_Interrupt          <= '0';
    elsif rising_edge(Host_Clock) then

      -- Default output assignments
      capture_Header <= '0';
      Host_Ack       <= '0';
      Host_ReadData  <= (others => '0');

      -- Delay taps
      host_Enable_d <= Host_Enable;

      -- Cache the present IRQ vectors
      new_Irq_Flags := irq_Flags_Reg;
      new_Irq_Mask  := irq_Mask_Reg;

      -- Clock enable host operations
      if(Host_Enable = '1') then
        -- Assume a default of a single cycle of latency for most transactions
        Host_Ack <= '1';
        case Host_Address is
          when BAYER_CTRL_REG_ADDRESS =>
            Host_ReadData(BAYER_ENABLE_BIT)   <= core_Enable_Reg;
            Host_ReadData(BAYER_COL_MODE_BIT) <= To_Std_Logic(column_Mode_Reg);
            Host_ReadData(BAYER_ROW_MODE_BIT) <= To_Std_Logic(row_Mode_Reg);
            if ?? Host_Write then
              core_Enable_Reg <= Host_WriteData(BAYER_ENABLE_BIT);
              column_Mode_Reg <= To_Column_Mode(Host_WriteData(BAYER_COL_MODE_BIT));
              row_Mode_Reg    <= To_Row_Mode(Host_WriteData(BAYER_ROW_MODE_BIT));
              capture_Header  <= Host_WriteData(CAPTURE_CTRL_BIT);
            end if;

          when BAYER_IRQ_FLAGS_REG_ADDRESS =>
            -- Return the present state of the IRQ flags, clearing any which are
            -- written with asserted bits
            Host_ReadData(irq_Flags_Reg'RANGE) <= irq_Flags_Reg;
            if ?? Host_Write then
              for irq_Index in irq_Flags_Reg'range loop
                if(Host_WriteData(irq_Index) = '1') then
                  new_Irq_Flags(irq_Index) := '0';
                end if;
              end loop;
            end if;
            
          when BAYER_IRQ_MASK_REG_ADDRESS =>
            -- Capture the new value for mask registers; a variable is used simply
            -- to reduce the detection of a required event by one clock.
            Host_ReadData(irq_Mask_Reg'RANGE) <= irq_Mask_Reg;
            if ?? Host_Write then
              new_Irq_Mask := Host_WriteData(new_Irq_Mask'RANGE);
            end if;

          -- Return the most-recent capture value for each video header register
          when BAYER_RES_CAPTURE_ADDRESS =>
            Host_ReadData <= (std_logic_vector(header_Frame_Res_Reg.y) &
                              std_logic_vector(header_Frame_Res_Reg.x));

          when BAYER_ROI_CAPTURE_ADDRESS =>
            Host_ReadData <= (std_logic_vector(header_Frame_Roi_Reg.y) &
                              std_logic_vector(header_Frame_Roi_Reg.x));
            
          when BAYER_INDEX_CAPTURE_ADDRESS =>
            Host_ReadData <= std_logic_vector(header_Frame_Index_Reg);
            
          when BAYER_TS_CAPTURE_ADDRESS =>
            Host_ReadData <= std_logic_vector(header_Frame_Timestamp_Reg);

          when BAYER_WHITE_BALANCE_RED =>
            Host_ReadData(White_Balance_Gain_Type'RANGE) <=
              std_logic_vector(white_Balance_Gain_Regs(RGB_PLANE_RED));
            if ?? Host_Write then
              white_Balance_Gain_Regs(RGB_PLANE_RED) <=
                unsigned(Host_WriteData(White_Balance_Gain_Type'RANGE));
            end if;
            
          when BAYER_WHITE_BALANCE_GREEN =>
            Host_ReadData(White_Balance_Gain_Type'RANGE) <=
                std_logic_vector(white_Balance_Gain_Regs(RGB_PLANE_GREEN));
            if ?? Host_Write then
              white_Balance_Gain_Regs(RGB_PLANE_GREEN) <=
                unsigned(Host_WriteData(White_Balance_Gain_Type'RANGE));
            end if;
            
          when BAYER_WHITE_BALANCE_BLUE =>
            Host_ReadData(White_Balance_Gain_Type'RANGE) <=
                std_logic_vector(white_Balance_Gain_Regs(RGB_PLANE_BLUE));
            if ?? Host_Write then
              white_Balance_Gain_Regs(RGB_PLANE_BLUE) <=
                unsigned(Host_WriteData(White_Balance_Gain_Type'RANGE));
            end if;
            
          when BAYER_ID_REG_ADDRESS =>
            Host_ReadData <= BAYER_ID_WORD;

          when BAYER_REVISION_REG_ADDRESS =>
            -- Encode the minor and major revision numbers
            field_Width := REVISION_FIELD_BITS;
            field_Low   := 0;
            field_High  := (field_Width - 1);
            Host_ReadData(field_High downto field_Low) <=
              std_logic_vector(To_Unsigned(BAYER_DEMOSAIC_REVISION_MINOR, REVISION_FIELD_BITS));
            field_Low  := (field_High + 1);
            field_High := (field_Low + field_Width - 1);
            Host_ReadData(field_High downto field_Low) <=
              std_logic_vector(To_Unsigned(BAYER_DEMOSAIC_REVISION_MAJOR, REVISION_FIELD_BITS));
              
          when BAYER_CAPS_REG_ADDRESS =>
            -- Reflect backpressure support with its assigned bit
            if(SUPPORTS_BACKPRESSURE) then
              Host_Readdata(BP_SUPPORT_CAPS_BIT) <= '1';
            end if;
            
            -- Encode the number of bits per pel, minus one
            field_Width := PEL_DEPTH_CAPS_BITS;
            field_Low   := 0;
            field_High  := (field_Width - 1);
            Host_ReadData(field_High downto field_Low) <=
              std_logic_vector(To_Unsigned((PEL_DEPTH - 1), field_Width));

            -- Encode the log2 of the number of pixels per clock
            field_Width := PIX_PER_CLK_CAPS_BITS;
            field_Low   := (field_High + 1);
            field_High  := (field_Low + field_Width - 1);
            Host_ReadData(field_High downto field_Low) <=
              std_logic_vector(To_Unsigned(Min_Repr_Bits(PIXELS_PER_CLOCK - 1), field_Width));
            
          when others =>
            
        end case;
      end if;

      -- Process interrupt flag events

      -- Detect video header capture completions
      if(capture_Complete = '1') then
        new_Irq_Flags(IRQ_CAPTURE) := '1';
      end if;

      -- Process interrupt request excitation based upon just-updated flags
      -- and / or mask bits
      Host_Interrupt <= '0';
      for irq_Index in new_Irq_Flags'RANGE loop
        if((new_Irq_Flags(irq_Index) and new_Irq_Mask(irq_Index)) = '1') then
          Host_Interrupt <= '1';
        end if;
      end loop;

      -- Commit the IRQ vector variables back to their respective signals
      irq_Flags_Reg <= new_Irq_Flags;
      irq_Mask_Reg  <= new_Irq_Mask;
      
    end if;
  end process;


  --
  -- Header capture logic
  --
  header_Logic : block

    -- Header capture state space
    signal capture_Armed : std_logic;

  begin
    
    -- Process to perform captures of video header fields.
    --
    -- Capture is requested via the control register, and trigger an IRQ flag
    -- upon completion. Header fields are captured from the next incoming frame.
    capture_Headers : process(Host_Reset, Host_Clock)
      variable capture_Cycle : boolean;
    begin
      if(Host_Reset = '1') then
        capture_Armed    <= '0';
        capture_Complete <= '0';
      elsif rising_edge(Host_Clock) then
        
        -- Default assignments
        capture_Cycle    := false;
        capture_Complete <= '0';

        -- Respond to capture requests coming from the control register
        if(capture_Header = '1') then
          -- Allow for capture within the same cycle
          if(Frame_Header_Strobe = '1') then
            capture_Armed <= '0';
            capture_Cycle := true;
          else
            capture_Armed <= '1';
          end if;
        else
          capture_Cycle := ((capture_Armed and Frame_Header_Strobe) = '1');
        end if;
        
        -- Capture the registers and indicate completion
        if capture_Cycle then
          -- Capture registers, which are guaranteed to be stable by the parser
          -- on this cycle
          header_Frame_Res_Reg       <= Frame_Resolution;
          header_Frame_Roi_Reg       <= Frame_Roi_Offset;
          header_Frame_Index_Reg     <= Frame_Index;
          header_Frame_Timestamp_Reg <= Frame_Timestamp;
          
          -- Reset the state post-capture
          capture_Armed    <= '0';
          capture_Complete <= '1';
        end if;
        
      end if;
    end process;

  end block;

  
  --
  -- Drive register file outputs
  --
  Core_Enable         <= core_Enable_Reg;
  Row_Mode            <= row_Mode_Reg;
  Column_Mode         <= column_Mode_Reg;
  White_Balance_Gains <= white_Balance_Gain_Regs;

end architecture;
