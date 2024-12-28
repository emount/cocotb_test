-- File Name   : Bayer_Demosaic_Parser.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Submodule performing extraction of run-time format details
--               from input headers, as well as parsing out the video frame
--               payload for consumption by downstream modules.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Module packages
library work;
use work.Bayer_Demosaic_Package.all;
use work.CL_Video_Defs.all;
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Parser is
  generic
  (
    -- PEL_DEPTH         - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK  - Number of horizontal pixel values per clock
    -- EMPTY_SYMBOL_BITS - Width of the input "empty symbols" vector, in bits
    PEL_DEPTH         : positive;
    PIXELS_PER_CLOCK  : positive;
    EMPTY_SYMBOL_BITS : positive
  );
  port
  (
    -- Input reset and clock
    Input_Reset : in std_logic;
    Input_Clock : in std_logic;

    -- Host reset and clock
    Host_Reset : in std_logic;
    Host_Clock : in std_logic;
    
    -- Header capture interface
    --
    -- Separate strobes are provided for each clock domain
    Frame_Header_Pixel : out std_logic;
    Frame_Header_Host  : out std_logic;
    Frame_Resolution   : out CL_Video_Coordinate;
    Frame_Roi_Offset   : out CL_Video_Coordinate;
    Frame_Index        : out CL_Video_Frame_Index;
    Frame_Timestamp    : out CL_Video_Frame_Timestamp;

    -- Input pixel stream
    Input_Valid         : in  std_logic;
    Input_Data          : in  std_logic_vector(((PIXELS_PER_CLOCK * PEL_LANE_BITS) - 1) downto 0);
    Input_StartOfPacket : in  std_logic;
    Input_EndOfPacket   : in  std_logic;
    Input_Empty         : in  std_logic_vector((EMPTY_SYMBOL_BITS - 1) downto 0);

    -- Payload output stream
    Payload_Valid  : out std_logic;
    Payload_Start  : out std_logic;
    Payload_End    : out std_logic;
    Payload_Pixels : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0)
  );
end entity;


-- Architecture implementation
architecture Rtl of Bayer_Demosaic_Parser is

  --
  -- Computed constants for slicing the header bit fields
  --
  -- The width of each header field and each pixel field are identical; this
  -- is used as a priori knowledge, rather than defining a more-complex
  -- relationship.
  --
  -- There is no existing use case / leeway in the frame packing spec to
  -- require otherwise.
  constant FIELDS_PER_CLOCK       : positive := PIXELS_PER_CLOCK;
  constant FRAME_RES_WORD         : natural  := (CL_VIDEO_FRAME_WIDTH_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_WIDTH_LOW        : natural  := ((CL_VIDEO_FRAME_WIDTH_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_HEIGHT_LOW       : natural  := ((CL_VIDEO_FRAME_HEIGHT_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_ROI_OFFSET_WORD  : natural  := (CL_VIDEO_ROI_OFFSET_X_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_ROI_OFFSET_X_LOW : natural  := ((CL_VIDEO_ROI_OFFSET_X_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_ROI_OFFSET_Y_LOW : natural  := ((CL_VIDEO_ROI_OFFSET_Y_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_INDEX_WORD       : natural  := (CL_VIDEO_FRAME_INDEX_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_INDEX_LOW        : natural  := ((CL_VIDEO_FRAME_INDEX_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_TIMESTAMP_WORD   : natural  := (CL_VIDEO_TIMESTAMP_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_TIMESTAMP_LOW    : natural  := ((CL_VIDEO_TIMESTAMP_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant PAYLOAD_START_WORD     : positive := (CL_VIDEO_PAYLOAD_START_FIELD / FIELDS_PER_CLOCK);

  -- Parser state space
  constant WORD_COUNT_BITS : positive := Min_Repr_Bits(PAYLOAD_START_WORD - 1);
  subtype Word_Count_Type is unsigned((WORD_COUNT_BITS - 1) downto 0);
  signal header_Count           : Word_Count_Type := To_Unsigned(0, Word_Count_Type'LENGTH);
  signal header_Count_tc        : std_logic;
  signal header_Cycle           : std_logic;
  signal payload_Cycle          : std_logic;
  signal payload_First          : std_logic;
  signal frame_Res_Cycle        : std_logic;
  signal frame_Roi_Offset_Cycle : std_logic;
  signal frame_Index_Cycle      : std_logic;
  signal frame_Timestamp_Cycle  : std_logic;
  signal frame_Header_Pixel_int : std_logic;
  signal frame_Header_Toggle    : std_logic;

  -- Data pipeline
  signal input_Valid_d : std_logic;
  signal input_Start_d : std_logic;
  signal input_End_d   : std_logic;
  signal input_Data_d  : std_logic_vector(Input_Data'RANGE);

  -- Header field registers
  signal frame_Resolution_Reg : CL_Video_Coordinate;
  signal frame_Roi_Offset_Reg : CL_Video_Coordinate;
  signal frame_Index_Reg      : CL_Video_Frame_Index;
  signal frame_Timestamp_Reg  : CL_Video_Frame_Timestamp;  
  
begin  

  --
  -- Header parsing process
  --
  header_Parse : process(Input_Reset, Input_Clock)
  begin
    if ?? Input_Reset then
      header_Count           <= To_Unsigned(0, header_Count'LENGTH);
      header_Count_tc        <= '1';
      header_Cycle           <= '0';
      payload_Cycle          <= '0';
      payload_First          <= '0';
      frame_Header_Toggle    <= '0';
      frame_Header_Pixel_int <= '0';
    elsif rising_edge(Input_Clock) then

      -- Default assignments
      frame_Header_Pixel_int <= '0';

      -- Pass along the delayed control flags
      input_Valid_d <= Input_Valid;
      input_Start_d <= Input_StartOfPacket;
      input_End_d   <= Input_EndOfPacket;
      
      -- Clock-enable the entire process with the active beat indication
      if ?? Input_Valid then
        -- Register the input data to break up timing and align with the
        -- word locator strobes, which are also registered to ease timing
        input_Data_d <= Input_Data;

        -- Sequentially decode cycles in which to capture header and payload
        -- data from the registered input vector
        if ?? Input_StartOfPacket then
          header_Cycle  <= '1';
          payload_Cycle <= '0';
          header_Count  <= To_Unsigned(0, header_Count'LENGTH);
          if(PAYLOAD_START_WORD = 1) then
            header_Count_tc <= '1';
          else
            header_Count_tc <= '0';
          end if;
        elsif not header_Count_tc then
          header_Count <= (header_Count + 1);
          if(header_Count = (PAYLOAD_START_WORD - 2)) then
            header_Count_tc <= '1';
          end if;
        else
          header_Cycle  <= '0';
          payload_Cycle <= '1';
          payload_First <= header_Cycle;
        end if;
      else
        payload_Cycle <= '0';
      end if;

      -- Capture the header fields as they are identified, a cycle after the
      -- active data beat
      if ?? input_Valid_d then
        if ?? frame_Res_Cycle then
          frame_Resolution_Reg.x <= CL_Video_Offset(input_Data_d((FRAME_WIDTH_LOW + CL_Video_Offset'HIGH) downto
                                                                 FRAME_WIDTH_LOW));
          frame_Resolution_Reg.y <= CL_Video_Offset(input_Data_d((FRAME_HEIGHT_LOW + CL_Video_Offset'HIGH) downto
                                                                 FRAME_HEIGHT_LOW));
        end if;
        
        if ?? frame_Roi_Offset_Cycle then
          frame_Roi_Offset_Reg.x <= CL_Video_Offset(input_Data_d((FRAME_ROI_OFFSET_X_LOW + CL_Video_Offset'HIGH) downto
                                                                 FRAME_ROI_OFFSET_X_LOW));
          frame_Roi_Offset_Reg.y <= CL_Video_Offset(input_Data_d((FRAME_ROI_OFFSET_Y_LOW + CL_Video_Offset'HIGH) downto
                                                                 FRAME_ROI_OFFSET_Y_LOW));
        end if;
        
        if ?? frame_Index_Cycle then
          frame_Index_Reg <= CL_Video_Frame_Index(input_Data_d((FRAME_INDEX_LOW + CL_Video_Frame_Index'HIGH) downto FRAME_INDEX_LOW));
        end if;
        
        -- Strobe out for one clock cycle when all the header registers are captured
        --
        -- Produce strobes in both the pixel and host clock domains
        frame_Header_Pixel_int <= frame_Timestamp_Cycle;
        if ?? frame_Timestamp_Cycle then
          frame_Header_Toggle <= not frame_Header_Toggle;
          frame_Timestamp_Reg <= CL_Video_Frame_Timestamp(input_Data_d((FRAME_TIMESTAMP_LOW + CL_Video_Frame_Timestamp'HIGH) downto FRAME_TIMESTAMP_LOW));
        end if;

      end if; -- if(active beat delay-one)

    end if;
  end process;


  -- Combinatorially decode the cycles in which the header fields may each
  -- be captured
  frame_Res_Cycle        <= header_Cycle when (header_Count = FRAME_RES_WORD) else '0';
  frame_Roi_Offset_Cycle <= header_Cycle when (header_Count = FRAME_ROI_OFFSET_WORD) else '0';
  frame_Index_Cycle      <= header_Cycle when (header_Count = FRAME_INDEX_WORD) else '0';
  frame_Timestamp_Cycle  <= header_Cycle when (header_Count = FRAME_TIMESTAMP_WORD) else '0';

  -- Drive out the frame-captured strobe for the pixel clock domain
  Frame_Header_Pixel <= frame_Header_Pixel_int;


  --
  -- Pipeline delay for the payload, since local logic requires fanout from
  -- the same delayed data tap.
  --
  payload_Delay : process(Input_Clock)
    -- Compute the least-significant bit index of actual pel data within each
    -- lane of input pel values, which are maximally-sized
    constant PEL_SLICE_LSB : natural := (PEL_LANE_BITS - PEL_DEPTH);
  begin
    if rising_edge(Input_Clock) then

      -- Systolically delay the qualifying flags
      Payload_Valid <= payload_Cycle;
      Payload_Start <= payload_First;
      Payload_End   <= input_End_d;

      -- Reduce the input pixel vectors from the maximal pel-lane depth with
      -- which they arrive at the input down to the actual pel bit depth
      -- specified for the instance.
      for pixel_Index in 0 to (PIXELS_PER_CLOCK - 1) loop
        Payload_Pixels((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
          input_Data_d((((pixel_Index + 1) * PEL_LANE_BITS) - 1) downto ((pixel_Index * PEL_LANE_BITS) + PEL_SLICE_LSB));
      end loop;
      
    end if;
  end process;


  --
  -- Host interface logic
  --
  host_Interface : block

    -- Signals for toggle-based clock domain crossing
    constant CDC_TOGGLE_TAPS     : positive := 5;
    signal frame_Header_Toggle_d : std_logic_vector(1 to CDC_TOGGLE_TAPS);
    signal frame_Header_Strobe   : std_logic;
    
  begin

     -- Safely strobe captured values of the registers into the host clock domain
     cross_Host_Regs : process(Host_Reset, Host_Clock)
     begin
       if ?? Host_Reset then
         frame_Header_Toggle_d <= (others => '0');
       elsif rising_edge(Host_Clock) then
         -- Update delay taps
         frame_Header_Toggle_d <=
           (frame_Header_Toggle & frame_Header_Toggle_d(1 to (frame_Header_Toggle_d'HIGH - 1)));
       end if;
     end process;

     frame_Header_Strobe <=
       (frame_Header_Toggle_d(frame_Header_Toggle_d'HIGH) xor
        frame_Header_Toggle_d(frame_Header_Toggle_d'HIGH - 1));
    
     -- Drive outputs to the register file
     Frame_Header_Host <= frame_Header_Strobe;
     Frame_Resolution  <= frame_Resolution_Reg;
     Frame_Roi_Offset  <= frame_Roi_Offset_Reg;
     Frame_Index       <= frame_Index_Reg;
     Frame_Timestamp   <= frame_Timestamp_Reg;
     
  end block;

end architecture;
