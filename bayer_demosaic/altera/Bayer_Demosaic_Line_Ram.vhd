-- File Name   : Bayer_Demosaic_Line_Ram.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Module implementing a single-line RAM buffer for the Bayer
--               demosaic core.
--
--               RAM is supported using inference, making this submodule
--               suitable for use across Altera families.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;


-- Entity declaration
entity Bayer_Demosaic_Line_Ram is
  generic
  (
    -- MAX_LINE_BEATS   - Maximum number of data beats per storage line
    -- PIXEL_BEAT_WIDTH - Width of a beat-worth of pixel data, in bits
    MAX_LINE_BEATS   : positive;
    PIXEL_BEAT_WIDTH : positive
  );
  port
  (
    -- Write interface
    Write_Clock   : in std_logic;
    Write_Enable  : in std_logic;
    Write_Strobe  : in std_logic;
    Write_Address : in integer range 0 to (MAX_LINE_BEATS - 1);
    Write_Data    : in std_logic_vector((PIXEL_BEAT_WIDTH - 1) downto 0);

    -- Read interface
    Read_Clock   : in  std_logic;
    Read_Enable  : in  std_logic;
    Read_Address : in  integer range 0 to (MAX_LINE_BEATS - 1);
    Read_Data    : out std_logic_vector((PIXEL_BEAT_WIDTH - 1) downto 0)
  );
end entity;


-- Architecture definition
architecture rtl of Bayer_Demosaic_Line_Ram is

  -- Memory array and register signals
  subtype Memory_Word_Type is std_logic_vector(Write_Data'RANGE);
  type Memory_Array_Type is array(0 TO (MAX_LINE_BEATS - 1)) of Memory_Word_Type;
  signal ram_Array        : Memory_Array_Type;
  signal read_Address_Reg : integer range 0 to (MAX_LINE_BEATS - 1) := 0;
  
begin

  --
  -- Write process
  --
  write_Logic : process (Write_Clock)
  begin
    if rising_edge(Write_Clock) then
      if ?? Write_Enable then
        if ?? Write_Strobe then
          ram_Array(Write_Address) <= Write_Data;
        end if;
      end if;
    end if;
  end process;


  --
  -- Read process
  read_Logic : process (Read_Clock)
  begin
    if rising_edge(Read_Clock) then
      if ?? Read_Enable then
        Read_Data        <= ram_Array(read_Address_Reg);
        read_Address_Reg <= Read_Address;
      end if;
    end if;
  end process;
  
end architecture;
