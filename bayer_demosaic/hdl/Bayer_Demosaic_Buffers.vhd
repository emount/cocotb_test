-- File Name   : Bayer_Demosaic_Buffers.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Module aggregating the line buffers needed for Bayer demosaic
--               pixel window generation.
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


-- Entity declaration
entity Bayer_Demosaic_Buffers is
  generic
  (
    -- NUM_LINES          - Number of lines to be be buffered and selected
    -- LINE_INDEX_BITS    - Number of bits in a line index vector
    -- BEAT_ADDRESS_WIDTH - Width of an intra-line beat address, in bits
    -- PIXEL_BEAT_WIDTH   - Width of a beat-worth of pixel data, in bits
    NUM_LINES          : positive;
    LINE_INDEX_BITS    : positive;
    BEAT_ADDRESS_WIDTH : positive;
    PIXEL_BEAT_WIDTH   : positive
  );
  port
  (
    -- Write interface
    Write_Clock   : in std_logic;
    Write_Enable  : in std_logic;
    Write_Index   : in unsigned((LINE_INDEX_BITS - 1) downto 0);
    Write_Address : in unsigned((BEAT_ADDRESS_WIDTH - 1) downto 0);
    Write_Data    : in std_logic_vector((PIXEL_BEAT_WIDTH - 1) downto 0);

    -- Read interface
    Read_Clock   : in  std_logic;
    Read_Enable  : in  std_logic;
    Read_Index   : in  unsigned((LINE_INDEX_BITS - 1) downto 0);
    Read_Address : in  unsigned((BEAT_ADDRESS_WIDTH - 1) downto 0);
    Read_Data    : out std_logic_vector((((NUM_LINES - 1) * PIXEL_BEAT_WIDTH) - 1) downto 0)
);
end entity;


-- Architecture definition
architecture rtl of Bayer_Demosaic_Buffers is

  -- Wrapper type declarations
  subtype Enable_Vector_Type is std_logic_vector((NUM_LINES - 1) downto 0);
  subtype Index_Type is unsigned(Read_Index'RANGE);
  type Index_Array_Type is array (natural range <>) of Index_Type;
  subtype Memory_Word_Type is std_logic_vector(Write_Data'RANGE);
  type Memory_Word_Array is array (natural range <>) of Memory_Word_Type;
  
  -- Write pipeline signals
  signal write_Enable_d  : std_logic;
  signal write_Enables   : Enable_Vector_Type;
  signal write_Address_d : unsigned(Write_Address'RANGE) := To_Unsigned(0, Write_Address'LENGTH);
  signal write_Data_d    : Memory_Word_Type;

  -- Read pipeline signals
  constant READ_PIPE_LATENCY : positive := 3;
  signal read_Enable_d    : std_logic_vector(1 to READ_PIPE_LATENCY);
  signal read_Enables     : Enable_Vector_Type;
  signal read_Enables_d   : Enable_Vector_Type;
  signal read_Enables_Ext : Enable_Vector_Type;
  signal read_Index_d     : Index_Array_Type(1 to READ_PIPE_LATENCY);
  signal read_Address_d   : unsigned(Read_Address'RANGE) := To_Unsigned(0, Read_Address'LENGTH);
  signal read_Column      : Memory_Word_Array(0 to (NUM_LINES - 1));

begin

  --
  -- Write multiplexing
  --
  write_Mux : process (Write_Clock)
  begin
    if rising_edge(Write_Clock) then
      -- Default output assignments
      write_Enables <= (others => '0');

      -- Update delay taps
      write_Enable_d <= Write_Enable;

      -- Respond to write cycles
      if ?? Write_Enable then
        -- Generate the appropriate RAM write select
        for line_Index in write_Enables'RANGE loop
          if(Write_Index = line_Index) then
            write_Enables(line_Index) <= '1';
          end if;
        end loop;

        -- Pipeline the write address and data
        write_Address_d <= Write_Address;
        write_Data_d    <= Write_Data;
      end if;
      
    end if;
  end process;


  --
  -- Instantiation of the line buffer RAM blocks
  --
  buffer_Block : block

    -- Wrapper signals shared by all buffer instances
    constant MAX_LINE_BEATS : positive := (2 ** BEAT_ADDRESS_WIDTH);
    subtype Integer_Address is integer range 0 to (MAX_LINE_BEATS - 1);
    signal read_Integer_Address  : Integer_Address;
    signal write_Integer_Address : Integer_Address;

  begin

    -- Continuously assign the integer-converted addresses
    write_Integer_Address <= To_Integer(write_Address_d);
    read_Integer_Address  <= To_Integer(read_Address_d);

    -- Replication of structural line buffer submodules
    line_Buffers : for line_Index in write_Enables'RANGE generate
    
      --
      -- Line buffer instance
      --
      line_Ram : entity work.Bayer_Demosaic_Line_Ram
      generic map
      (
        MAX_LINE_BEATS   => MAX_LINE_BEATS,
        PIXEL_BEAT_WIDTH => PIXEL_BEAT_WIDTH
      )
      port map
      (
        -- Write interface
        Write_Clock   => Write_Clock,
        Write_Enable  => write_Enables(line_Index),
        Write_Strobe  => write_Enables(line_Index),
        Write_Address => write_Integer_Address,
        Write_Data    => write_Data_d,
      
        -- Read interface
        Read_Clock   => Read_Clock,
        Read_Enable  => read_Enables_Ext(line_Index),
        Read_Address => read_Integer_Address,
        Read_Data    => read_Column(line_Index)
      );
    
    end generate;
    
  end block;


  --
  -- Read multiplexing
  --
  read_Mux : process (Read_Clock)
    variable wrapped_Index : natural;
  begin
    if rising_edge(Read_Clock) then
      
      -- Default output assignments
      read_Enables <= (others => '0');

      -- Update delay taps
      read_Enable_d <= (Read_Enable & read_Enable_d(1 to (read_Enable_d'HIGH - 1)));
      read_Index_d  <= (Read_Index & read_Index_d(1 to (read_Index_d'HIGH - 1)));

      -- Respond to read cycles
      if ?? Read_Enable then
        -- Generate the appropriate RAM read selects; it is necessary to enable
        -- all but one of the RAMs for reading
        read_Enables <= (others => '0');
        for line_Index in 0 to (read_Enables'HIGH - 1) loop
          wrapped_Index := ((To_Integer(Read_Index) + (NUM_LINES - line_Index)) mod NUM_LINES);
          read_Enables(wrapped_Index) <= '1';
        end loop;

        -- Pipeline the read address to present alongside the decoded read enables
        read_Address_d <= Read_Address;
      end if; -- if(read enable)

      
      -- Multiplex the set of lines indicated by the read index presented alongside
      -- the beat addresses
      if ?? read_Enable_d(read_Enable_d'HIGH) then
        -- Loop through all the enabled read data, presenting a properly-rotated
        -- column of data consisting of rows leading up to the base index.
        for line_Index in 0 to (NUM_LINES - 2) loop
          wrapped_Index := ((To_Integer(read_Index_d(read_Index_d'HIGH)) + (line_Index + (NUM_LINES - 2))) mod NUM_LINES);
          Read_Data((((line_Index + 1) * PIXEL_BEAT_WIDTH) - 1) downto (line_Index * PIXEL_BEAT_WIDTH)) <=
            read_Column(wrapped_Index);
        end loop;
      end if; -- if(delay read valid)

      -- Delay tap for the read enables; this is needed to drain the pipeline
      -- on the final beat of each line
      read_Enables_d <= read_Enables;

    end if;
  end process;

  -- Provide extended read strobes to each RAM line
  read_Enables_Ext <= (read_Enables or read_Enables_d);
  
end architecture;
