-- File Name   : bayer_demosaic.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Top-level wrapper for the bayer_demosaic core for use
--               in Intel Platform Designer.
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
use work.Bayer_Demosaic_Kernels.all;
use work.Bayer_Demosaic_Package.all;
use work.CL_Video_Defs.all;


-- Entity declaration
entity bayer_demosaic is
  generic
  (
    -- FPGA_FAMILY           - String indicating the FPGA family to compile for
    -- PEL_DEPTH             - Depth of each pixel element, in bits
    --                         Pels are always MSB-justified into max-depth bit lanes
    -- PIXELS_PER_CLOCK      - Number of horizontal pixel values per clock
    -- SUPPORTS_BACKPRESSURE - Backpressure support flag
    --                         0 = Core does not support downstream backpressure
    --                         1 = Backpressure FIFO logic included for Output_Ready
    -- EMPTY_SYMBOL_BITS     - Width of the input "empty symbols" vector, in bits
    -- INTERP_KERNEL_IDX     - Index of the interpolation kernel to employ
    --                         (Refer to Bayer_Demosaic_Kernels.vhd)
    -- MAX_HORIZ_RES         - Maximum supported horizontal resolution, in pixels
    FPGA_FAMILY           : string   := "";
    PEL_DEPTH             : positive := 8;
    PIXELS_PER_CLOCK      : positive := 16;
    SUPPORTS_BACKPRESSURE : natural  := 0;
    EMPTY_SYMBOL_BITS_IN  : positive := 4;
    EMPTY_SYMBOL_BITS_OUT : positive := 4;
    INTERP_KERNEL_IDX     : natural  := 1;
    MAX_HORIZ_RES         : positive := 4096
  );
  port
  (
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
    Input_StartOfPacket : in  std_logic;
    Input_EndOfPacket   : in  std_logic;
    Input_Empty         : in  std_logic_vector((EMPTY_SYMBOL_BITS_IN - 1) downto 0) := (others=>'0');
    Input_Data          : in  std_logic_vector(((PIXELS_PER_CLOCK * PEL_LANE_BITS) - 1) downto 0);
    Input_Ready         : out std_logic;

    -- Output reset and clock
    Output_Reset : in std_logic;
    Output_Clock : in std_logic;

    -- Output pixel stream
    Output_Valid         : out std_logic;
    Output_StartOfPacket : out std_logic;
    Output_EndOfPacket   : out std_logic;
    Output_Empty         : out std_logic_vector((EMPTY_SYMBOL_BITS_OUT - 1) downto 0);
    Output_Data          : out std_logic_vector(((PIXELS_PER_CLOCK * NUM_RGB_PLANES * PEL_LANE_BITS) - 1) downto 0);
    Output_Ready         : in  std_logic := '1'
  );
end entity;


-- Architecture definition
architecture rtl of bayer_demosaic is

  -- Derived constants
  constant INTERP_KERNEL : Bayer_Kernel_Type := To_Bayer_Kernel_Type(INTERP_KERNEL_IDX);

  -- Host interface wrapper signals
  signal host_Enable : std_logic;
  signal host_Ack    : std_logic;

  -- Global register file outputs
  signal core_Enable : std_logic;

  -- Reset logic outputs
  signal input_Reset_Sync  : std_logic;
  signal output_Reset_Sync : std_logic;

  -- Interface signals between the input stream parser and the register file
  signal frame_Header_Input : std_logic;
  signal frame_Header_Host  : std_logic;
  signal frame_Resolution   : CL_Video_Coordinate;
  signal frame_Roi_Offset   : CL_Video_Coordinate;
  signal frame_Index        : CL_Video_Frame_Index;
  signal frame_Timestamp    : CL_Video_Frame_Timestamp;

  -- Interface signals between the register file and the demosaic pipeline
  signal row_Mode            : Row_Mode_Type;
  signal column_Mode         : Column_Mode_Type;
  signal white_Balance_Gains : White_Balance_Gain_Array((NUM_RGB_PLANES - 1) downto 0);

  -- Payload stream from parser
  signal payload_Valid  : std_logic;
  signal payload_Start  : std_logic;
  signal payload_End    : std_logic;
  signal payload_Pixels : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal payload_Ready  : std_logic;

  -- Forwarded header fields and output header trigger
  signal header_Trigger    : std_logic;
  signal header_Resolution : CL_Video_Coordinate;
  signal header_Roi_Offset : CL_Video_Coordinate;
  signal header_Index      : CL_Video_Frame_Index;
  signal header_Timestamp  : CL_Video_Frame_Timestamp;
  
  -- Bayer-processed pixel stream
  signal bayer_Valid : std_logic;
  signal bayer_Start : std_logic;
  signal bayer_End   : std_logic;
  signal bayer_Red   : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal bayer_Green : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal bayer_Blue  : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal bayer_Ready : std_logic;

  -- Backpressure signals from the output sink
  signal sink_Ready : std_logic;
  
begin

  --
  -- Host register interface
  --
  registers : entity work.Bayer_Demosaic_Registers
  generic map
  (
    PEL_DEPTH             => PEL_DEPTH,
    PIXELS_PER_CLOCK      => PIXELS_PER_CLOCK,
    SUPPORTS_BACKPRESSURE => (SUPPORTS_BACKPRESSURE /= 0)
  )
  port map
  (
    -- Host reset and clock 
    Host_Reset => Host_Reset,
    Host_Clock => Host_Clock,
   
    -- Host control interface
    Host_Enable     => host_Enable,
    Host_Address    => Host_Address,
    Host_Write      => Host_Write,
    Host_WriteData  => Host_WriteData,
    Host_ByteEnable => Host_ByteEnable,
    Host_ReadData   => Host_ReadData,
    Host_Ack        => host_Ack,
    Host_Interrupt  => Host_Interrupt,

    -- Global register file outputs
    Core_Enable => core_Enable,

    -- Interface to input stream parser
    Frame_Header_Strobe => frame_Header_Host,
    Frame_Resolution    => frame_Resolution,
    Frame_Roi_Offset    => frame_Roi_Offset,
    Frame_Index         => frame_Index,
    Frame_Timestamp     => frame_Timestamp,

    -- Interface to demosaic pipeline
    Row_Mode            => row_Mode,
    Column_Mode         => column_Mode,
    White_Balance_Gains => white_Balance_Gains
  );

  -- Some minor glue logic for the bus interface
  host_Enable <= (Host_Read or Host_Write);


  --
  -- Reset control logic for the pipeline
  --
  reset_Sync : entity work.Bayer_Demosaic_Reset
  port map
  (
    -- Input reset and clock 
    Input_Reset => Input_Reset,
    Input_Clock => Input_Clock,
   
    -- Output reset and clock 
    Output_Reset => Output_Reset,
    Output_Clock => Output_Clock,
   
    -- Host control interface
    Core_Enable => core_Enable,

    -- Synchronized reset outputs
    Input_Reset_Sync  => input_Reset_Sync,
    Output_Reset_Sync => output_Reset_Sync
  );


  --
  -- Input stream parsing
  --
  --
  -- The parser submodule extracts run-time format information from the input
  -- stream headers, and delimits the valid beats of input video frame data
  -- for the cascade of downstream processing modules
  --
  parser : entity work.Bayer_Demosaic_Parser
  generic map
  (
    PEL_DEPTH         => PEL_DEPTH,
    PIXELS_PER_CLOCK  => PIXELS_PER_CLOCK,
    EMPTY_SYMBOL_BITS => EMPTY_SYMBOL_BITS_IN
  )
  port map
  (
    -- Input reset and clock
    Input_Reset => input_Reset_Sync,
    Input_Clock => Input_Clock,

    -- Host reset and clock
    Host_Reset => Host_Reset,
    Host_Clock => Host_Clock,
    
    -- Header capture interface
    Frame_Header_Pixel => frame_Header_Input,
    Frame_Header_Host  => frame_Header_Host,
    Frame_Resolution   => frame_Resolution,
    Frame_Roi_Offset   => frame_Roi_Offset,
    Frame_Index        => frame_Index,
    Frame_Timestamp    => frame_Timestamp,

    -- Input pixel stream
    Input_Valid         => Input_Valid,
    Input_Data          => Input_Data,
    Input_StartOfPacket => Input_StartOfPacket,
    Input_EndOfPacket   => Input_EndOfPacket,
    Input_Empty         => Input_Empty,

    -- Payload output stream
    Payload_Valid  => payload_Valid,
    Payload_Start  => payload_Start,
    Payload_End    => payload_End,
    Payload_Pixels => payload_Pixels
  );


  --
  -- Bayer demosaic processing pipeline
  --
  pipeline : entity work.Bayer_Demosaic_Pipeline
  generic map
  (
    PEL_DEPTH        => PEL_DEPTH,
    PIXELS_PER_CLOCK => PIXELS_PER_CLOCK,
    INTERP_KERNEL    => INTERP_KERNEL,
    MAX_HORIZ_RES    => MAX_HORIZ_RES
  )
  port map
  (
    -- Input reset and clock
    Input_Reset => input_Reset_Sync,
    Input_Clock => Input_Clock,

    -- Interface to the register file
    Row_Mode            => row_Mode,
    Column_Mode         => column_Mode,
    White_Balance_Gains => white_Balance_Gains,

    -- Header capture signals from the parser
    Frame_Header_Strobe => frame_Header_Input,
    Frame_Resolution    => frame_Resolution,
    Frame_Roi_Offset    => frame_Roi_Offset,
    Frame_Index         => frame_Index,
    Frame_Timestamp     => frame_Timestamp,

    -- Payload input stream
    Payload_Valid  => payload_Valid,
    Payload_Start  => payload_Start,
    Payload_End    => payload_End,
    Payload_Pixels => payload_Pixels,

    -- Output reset and clock
    Output_Reset => output_Reset_Sync,
    Output_Clock => Output_Clock,

    -- Ready signal from the output interface
    Output_Ready => Output_Ready,

    -- Forwarded header fields and output header trigger
    Header_Trigger    => header_Trigger,
    Header_Resolution => header_Resolution,
    Header_Roi_Offset => header_Roi_Offset,
    Header_Index      => header_Index,
    Header_Timestamp  => header_Timestamp,

    -- Bayer-processed pixel stream
    Bayer_Valid => bayer_Valid,
    Bayer_Start => bayer_Start,
    Bayer_End   => bayer_End,
    Bayer_Red   => bayer_Red,
    Bayer_Green => bayer_Green,
    Bayer_Blue  => bayer_Blue
  );
  
  
  --
  -- Output stream encapsulation
  --
  -- The video header values captured at the input by the parser are used to
  -- encapsulate the processed Bayer-processed stream with an identical, time-
  -- delayed header.
  --
  -- Backpressure and pipeline inertia are managed within this module, which
  -- also forwards along a pipelined version of the output ready signal to
  -- the input stream interface.
  encapsulator : entity work.Bayer_Demosaic_Encapsulator
  generic map
  (
    FPGA_FAMILY       => FPGA_FAMILY,
    PEL_DEPTH         => PEL_DEPTH,
    PIXELS_PER_CLOCK  => PIXELS_PER_CLOCK,
    EMPTY_SYMBOL_BITS => EMPTY_SYMBOL_BITS_OUT,
    BYPASS_ELASTIC    => (SUPPORTS_BACKPRESSURE = 0)
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => output_Reset_Sync,
    Pixel_Clock => Output_Clock,

    -- Header fields and triggering interface
    Header_Trigger    => header_Trigger,
    Header_Resolution => header_Resolution,
    Header_Roi_Offset => header_Roi_Offset,
    Header_Index      => header_Index,
    Header_Timestamp  => header_Timestamp,

    -- Bayer-processed pixel stream
    Bayer_Valid => bayer_Valid,
    Bayer_Red   => bayer_Red,
    Bayer_Green => bayer_Green,
    Bayer_Blue  => bayer_Blue,
    Bayer_Start => bayer_Start,
    Bayer_End   => bayer_End,

    -- Pipelined backpressure indication
    Sink_Ready => sink_Ready,

    -- Output pixel stream
    Output_Valid         => Output_Valid,
    Output_Data          => Output_Data,
    Output_StartOfPacket => Output_StartOfPacket,
    Output_EndOfPacket   => Output_EndOfPacket,
    Output_Empty         => Output_Empty,
    Output_Ready         => Output_Ready
  );

  
  -- Safely synchronize the output ready signal to the input domain
  sync_Ready : block

    -- Synchronization delay line
    constant SYNC_DELAY_TAPS : positive := 3;
    signal sink_Ready_d      : std_logic_vector(1 to SYNC_DELAY_TAPS) := (others => '0');

  begin

    -- Triple-sync the ready signal and forward to the input
    sink_Ready_d <= (sink_Ready & sink_Ready_d(1 to (sink_Ready_d'HIGH - 1))) when
                    rising_edge(Input_Clock);
    Input_Ready  <= sink_Ready_d(sink_Ready_d'HIGH);

  end block;
  
end architecture;
