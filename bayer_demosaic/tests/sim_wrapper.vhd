-- File Name   : sim_wrapper_template.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Simulation wrapper for parameterizing the top level of the
--               Bayer demosaic core for different test permutations via
--               generics.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;

-- Module packages
library work;
use work.Bayer_Demosaic_Package.all;
use work.Hdl_Utilities.all;


-- Entity declaration
entity sim_wrapper is
  port
  (
    -- Test harness interface
    Same_Clocks : in std_logic;
    
    -- Host reset and clock 
    Host_Reset : in std_logic;
    Host_Clock : in std_logic;
   
    -- Host control interface
    Host_Address    : in  Bayer_Host_Address;
    Host_Write      : in  std_logic;
    Host_WriteData  : in  std_logic_vector(31 downto 0);
    Host_ByteEnable : in  std_logic_vector(3 downto 0);
    Host_Read       : in  std_logic;
    Host_ReadData   : out std_logic_vector(31 downto 0);
    Host_Interrupt  : out std_logic;

    -- Input reset and clock
    Input_Reset : in std_logic;
    Input_Clock : in std_logic;

    -- Input pixel stream
    Input_Valid         : in  std_logic;
    Input_Data          : in  std_logic_vector(255 downto 0);
    Input_StartOfPacket : in  std_logic;
    Input_EndOfPacket   : in  std_logic;
    Input_Empty         : in  std_logic_vector(31 downto 0);
    Input_Ready         : out std_logic;

    -- Output reset and clock
    Output_Reset : in std_logic;
    Output_Clock : in std_logic;

    -- Output pixel stream
    Output_Valid         : out std_logic;
    Output_Data          : out std_logic_vector(767 downto 0);
    Output_StartOfPacket : out std_logic;
    Output_EndOfPacket   : out std_logic;
    Output_Empty         : out std_logic_vector(95 downto 0);
    Output_Ready         : in  std_logic
  );
end entity;


-- Architecture definition
architecture structural of sim_wrapper is

  -- Substitution targets for top-level generics to the DUT
  constant FPGA_FAMILY           : string   := "Arria V";
  constant PEL_DEPTH             : positive := 16;
  constant PIXELS_PER_CLOCK      : positive := 16;
  constant SUPPORTS_BACKPRESSURE : natural  := 0;
  constant INTERP_KERNEL_IDX     : natural  := 1;
  constant MAX_HORIZ_RES         : positive := 4096;

  -- These derived constants are computed by the core's HW-Tcl elaborate()
  -- method during system-level generation. Per comments in the TCL code, other
  -- IP cores expect symbols to be equivalent to bytes. Each pixel field is
  -- packed as 16 bits, resulting in a factor of two for symbols.
  constant EMPTY_SYMBOL_BITS_IN  : positive := (31 + 1);
  constant EMPTY_SYMBOL_BITS_OUT : positive := (95 + 1);

  -- Signals for multiplexing the output clock
  constant SAME_CLOCKS_DELAY : time := 1 ns;
  signal output_Clock_Muxed  : std_logic := '0';
  
begin

  --
  -- Multiplexer for the output clock mode, with respect to the input
  --
  output_Clock_Muxed <= transport Input_Clock after SAME_CLOCKS_DELAY when
                          (Same_Clocks = '1') else
                          Output_Clock;

  
  --
  -- Instantiate the device under test with the rendered generics
  --
  dut : entity work.bayer_demosaic
  generic map
  (
    FPGA_FAMILY           => FPGA_FAMILY,
    PEL_DEPTH             => PEL_DEPTH,
    PIXELS_PER_CLOCK      => PIXELS_PER_CLOCK,
    SUPPORTS_BACKPRESSURE => SUPPORTS_BACKPRESSURE,
    EMPTY_SYMBOL_BITS_IN  => EMPTY_SYMBOL_BITS_IN,
    EMPTY_SYMBOL_BITS_OUT => EMPTY_SYMBOL_BITS_OUT,
    INTERP_KERNEL_IDX     => INTERP_KERNEL_IDX,
    MAX_HORIZ_RES         => MAX_HORIZ_RES
  )
  port map
  (
    -- Host reset and clock 
    Host_Reset => Host_Reset,
    Host_Clock => Host_Clock,
   
    -- Host control interface
    Host_Address    => Host_Address,
    Host_Write      => Host_Write,
    Host_WriteData  => Host_WriteData,
    Host_ByteEnable => Host_ByteEnable,
    Host_Read       => Host_Read,
    Host_ReadData   => Host_ReadData,
    Host_Interrupt  => Host_Interrupt,

    -- Input reset and clock
    Input_Reset => Input_Reset,
    Input_Clock => Input_Clock,

    -- Input pixel stream
    Input_Valid         => Input_Valid,
    Input_StartOfPacket => Input_StartOfPacket,
    Input_EndOfPacket   => Input_EndOfPacket,
    Input_Empty         => Input_Empty,
    Input_Data          => Input_Data,
    Input_Ready         => Input_Ready,

    -- Output reset and clock
    Output_Reset => Output_Reset,
    Output_Clock => output_Clock_Muxed,

    -- Output pixel stream
    Output_Valid         => Output_Valid,
    Output_StartOfPacket => Output_StartOfPacket,
    Output_EndOfPacket   => Output_EndOfPacket,
    Output_Empty         => Output_Empty,
    Output_Data          => Output_Data,
    Output_Ready         => Output_Ready
  );
  
end architecture;
