-- File Name   : Bayer_Demosaic_Pipeline.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Submodule responsible for the actual Bayer interpolation.
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
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Pipeline is
  generic
  (
    -- PEL_DEPTH        - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK - Number of horizontal pixel values per clock
    -- INTERP_KERNEL    - Interpolation kernel to implement
    -- MAX_HORIZ_RES    - Maximum supported horizontal resolution, in pixels
    PEL_DEPTH        : positive;
    PIXELS_PER_CLOCK : positive;
    INTERP_KERNEL    : Bayer_Kernel_Type;
    MAX_HORIZ_RES    : positive
  );
  port
  (
    -- Input reset and clock
    Input_Reset : in std_logic;
    Input_Clock : in std_logic;

    -- Interface to the register file
    Row_Mode            : in Row_Mode_Type;
    Column_Mode         : in Column_Mode_Type;
    White_Balance_Gains : in White_Balance_Gain_Array((NUM_RGB_PLANES - 1) downto 0);

    -- Header capture signals from the parser
    Frame_Header_Strobe : in std_logic;
    Frame_Resolution    : in CL_Video_Coordinate;
    Frame_Roi_Offset    : in CL_Video_Coordinate;
    Frame_Index         : in CL_Video_Frame_Index;
    Frame_Timestamp     : in CL_Video_Frame_Timestamp;

    -- Payload input stream
    Payload_Valid  : in std_logic;
    Payload_Start  : in std_logic;
    Payload_End    : in std_logic;
    Payload_Pixels : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);

    -- Output reset and clock
    Output_Reset : in std_logic;
    Output_Clock : in std_logic;

    -- Ready signal from the output interface
    Output_Ready : in std_logic;

    -- Forwarded header fields and output header trigger
    Header_Trigger    : out std_logic;
    Header_Resolution : out CL_Video_Coordinate;
    Header_Roi_Offset : out CL_Video_Coordinate;
    Header_Index      : out CL_Video_Frame_Index;
    Header_Timestamp  : out CL_Video_Frame_Timestamp;
    
    -- Bayer-processed output RGB stream
    Bayer_Valid : out std_logic;
    Bayer_Start : out std_logic;
    Bayer_End   : out std_logic;
    Bayer_Red   : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Bayer_Green : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Bayer_Blue  : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0)
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Demosaic_Pipeline is

  -- Signals for forwarding input header field values
  signal header_Toggle     : std_logic;
  signal latest_Resolution : CL_Video_Coordinate;
  signal latest_Roi_Offset : CL_Video_Coordinate;
  signal latest_Index      : CL_Video_Frame_Index;
  signal latest_Timestamp  : CL_Video_Frame_Timestamp;

  -- Pipeline constants
  --
  -- The number of lines is chosen to satisfy the requirements of the core
  -- interpolation kernel configured for the instance. This is by definition
  -- always sufficient for that of the nearest-neighbor kernel unconditionally
  -- employed for boundary pixels. One additional line is added to permit the
  -- output reads and input writes to overlap without buffer contention.
  constant PACKED_WIDTH       : positive := (NUM_RGB_PLANES * Payload_Pixels'LENGTH);
  constant KERNEL_WINDOW_SIZE : positive := Bayer_Kernel_Window_Size(INTERP_KERNEL);

  -- Delay between the decision to start the first line of window generation
  -- and when the window actually begins. This is used to ensure there are
  -- adequate cycles to accommodate the output frame header encapsulation.
  -- Set to this value, the worst-case number of header data beats (for the
  -- minimum value of PIXELS_PER_CLOCK, 2) is just accommodated before the
  -- first data arrives when the lowest-latency kernel is used (nearest-
  -- neighbor) for all pixels.
  constant WINDOW_START_DELAY : positive := 1;

  -- Difference in the latency between the configured kernel and the nearest-
  -- neighbor kernel used for boundary pixels. If the NN kernel is itself
  -- configured, this resolves to zero.
  constant CORE_KERNEL_LATENCY : positive := Bayer_Kernel_Latency(INTERP_KERNEL);
  constant LATENCY_DIFFERENCE  : natural  := (CORE_KERNEL_LATENCY - BAYER_NEAREST_LATENCY);

  -- Window generator output signals
  signal window_Starting        : std_logic;
  signal window_Valid           : std_logic;
  signal window_Pixels          : std_logic_vector((((KERNEL_WINDOW_SIZE ** 2) * PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal window_Top             : std_logic;
  signal window_Bottom          : std_logic;
  signal window_Replicate_Row   : std_logic;
  signal window_Replicate_Left  : std_logic;
  signal window_Replicate_Right : std_logic;
  signal window_Red_Row         : std_logic;
  signal window_Green_First     : std_logic;
  
  -- Signals from the nearest-neighbor interpolation kernel
  signal nn_Interp_Valid : std_logic;
  signal nn_Interp_Start : std_logic;
  signal nn_Interp_End   : std_logic;
  signal nn_Interp_Red   : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal nn_Interp_Green : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal nn_Interp_Blue  : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  
  -- Signals from the core interpolation kernel
  signal core_Interp_Valid : std_logic;
  signal core_Interp_Start : std_logic;
  signal core_Interp_End   : std_logic;
  signal core_Interp_Red   : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal core_Interp_Green : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal core_Interp_Blue  : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  
  -- "Raw" (pre-balanced) multiplexed pixel stream
  signal bayer_Raw_Valid : std_logic;
  signal bayer_Raw_Start : std_logic;
  signal bayer_Raw_End   : std_logic;
  signal bayer_Raw_Red   : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal bayer_Raw_Green : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
  signal bayer_Raw_Blue  : std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    
begin

  capture_Header : process(Input_Reset, Input_Clock)
  begin
    if ?? Input_Reset then
      header_Toggle <= '0';
    elsif rising_edge(Input_Clock) then
      -- Capture input header fields whenever they are updated by the parser, and
      -- propagate them to the output domain for forwarding with the window.
      if ?? Frame_Header_Strobe then
        header_Toggle     <= not header_Toggle;
        latest_Resolution <= Frame_Resolution;
        latest_Roi_Offset <= Frame_Roi_Offset;
        latest_Index      <= Frame_Index;
        latest_Timestamp  <= Frame_Timestamp;
      end if;
    end if;
  end process;


  --
  -- Pixel "window" generator for the interpolation kernel
  --
  -- This is sized to accommodate the highest-complexity interpolation
  -- kernel in use, in terms of input dimension space. If a kernel more
  -- complex than nearest-neighbor is used for the core algorithm, it is
  -- fed in synchronization with the nearest-neighbor (which is used for
  -- boundary pixels on an unconditional basis.)
  --
  window_Generator : entity work.Bayer_Demosaic_Window
  generic map
  (
    PEL_DEPTH          => PEL_DEPTH,
    PIXELS_PER_CLOCK   => PIXELS_PER_CLOCK,
    KERNEL_WINDOW_SIZE => KERNEL_WINDOW_SIZE,
    MAX_HORIZ_RES      => MAX_HORIZ_RES,
    START_DELAY        => WINDOW_START_DELAY
  )
  port map
  (
    -- Input reset and clock
    Input_Reset => Input_Reset,
    Input_Clock => Input_Clock,

    -- Interface to the register file
    Row_Mode    => Row_Mode,
    Column_Mode => Column_Mode,

    -- Input resolution and pixel stream
    Input_Resolution => latest_Resolution,
    Input_Valid      => Payload_Valid,
    Input_Start      => Payload_Start,
    Input_End        => Payload_End,
    Input_Pixels     => Payload_Pixels,
    
    -- Output reset and clock
    Output_Reset => Output_Reset,
    Output_Clock => Output_Clock,

    -- Ready signal from the output interface
    Output_Ready => Output_Ready,

    -- Strobe indicating when the window is starting a new frame
    Output_Starting => window_Starting,

    -- Generated output window interface
    Output_Valid           => window_Valid,
    Output_Top             => window_Top,
    Output_Bottom          => window_Bottom,
    Output_Replicate_Row   => window_Replicate_Row,
    Output_Replicate_Left  => window_Replicate_Left,
    Output_Replicate_Right => window_Replicate_Right,
    Output_Red_Row         => window_Red_Row,
    Output_Green_First     => window_Green_First,
    Output_Pixels          => window_Pixels
  );


  --
  -- Nearest-neighbor interpolation kernel
  --
  -- This kernel is used unconditionally for all boundary pixels, where the
  -- Bayer patterning is such that there are insufficient pixels to perform
  -- any higher-order interpolation.
  --
  -- The kernel is configured to incur an additional systolic delay in order
  -- to align its output data beats exactly with those of the core kernel, if
  -- there is one.
  --
  nn_Kernel : entity work.Bayer_Kernel(Nearest_Neighbor)
  generic map
  (
    PIXELS_PER_CLOCK => PIXELS_PER_CLOCK,
    PLANE_DEPTH      => PEL_DEPTH,
    WINDOW_SIZE      => KERNEL_WINDOW_SIZE,
    EXTRA_LATENCY    => LATENCY_DIFFERENCE
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => Output_Reset,
    Pixel_Clock => Output_Clock,

    -- Pixel window for interpolation
    Window_Valid           => window_Valid,
    Window_Top             => window_Top,
    Window_Bottom          => window_Bottom,
    Window_Replicate_Row   => window_Replicate_Row,
    Window_Replicate_Left  => window_Replicate_Left,
    Window_Replicate_Right => window_Replicate_Right,
    Window_Red_Row         => window_Red_Row,
    Window_Green_First     => window_Green_First,
    Window_Pixels          => window_Pixels,

    -- Interpolated output pixel stream
    Output_Valid  => nn_Interp_Valid,
    Output_Start  => nn_Interp_Start,
    Output_End    => nn_Interp_End,
    Output_Red    => nn_Interp_Red,
    Output_Green  => nn_Interp_Green,
    Output_Blue   => nn_Interp_Blue
  );


  --
  -- Core interpolation kernel
  --
  -- This kernel is used for all of the inner pixels of each frame, past
  -- the boundary at which only nearest-neighbor can be implemented.
  --
  -- The inner architecture of the kernel is statically selected at compile
  -- time based upon the value of the INTERP_KERNEL generic.
  --
  core_Kernel : entity work.Bayer_Arch_Selector_Kernel
  generic map
  (
    INTERP_KERNEL    => INTERP_KERNEL,
    PIXELS_PER_CLOCK => PIXELS_PER_CLOCK,
    PLANE_DEPTH      => PEL_DEPTH,
    WINDOW_SIZE      => KERNEL_WINDOW_SIZE
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => Output_Reset,
    Pixel_Clock => Output_Clock,

    -- Pixel window for interpolation
    Window_Valid           => window_Valid,
    Window_Top             => window_Top,
    Window_Bottom          => window_Bottom,
    Window_Replicate_Row   => window_Replicate_Row,
    Window_Replicate_Left  => window_Replicate_Left,
    Window_Replicate_Right => window_Replicate_Right,
    Window_Red_Row         => window_Red_Row,
    Window_Green_First     => window_Green_First,
    Window_Pixels          => window_Pixels,

    -- Interpolated output pixel stream
    Output_Valid  => core_Interp_Valid,
    Output_Start  => core_Interp_Start,
    Output_End    => core_Interp_End,
    Output_Red    => core_Interp_Red,
    Output_Green  => core_Interp_Green,
    Output_Blue   => core_Interp_Blue
  );


  --
  -- Process to forward along header information to downstream logic in a
  -- manner synchronous with the window generation, and timed to coincide
  -- with the cycles just before the first pixel of each frame.
  --
  forward_Block : block

    -- Forwarding state space
    type Forward_State_Type is (FORWARD_IDLE, FORWARD_ARMED);
    signal forward_State      : Forward_State_Type;
    constant TOGGLE_SYNC_TAPS : positive := 4;
    signal header_Toggle_d    : std_logic_vector(1 to TOGGLE_SYNC_TAPS);

    -- The delay at which to trigger the header-generation process is related
    -- number of header beats required for the given PIXELS_PER_CLOCK configuration,
    -- as well as the core interpolation kernel latency.
    constant MAX_HEADER_BEATS     : positive := (CL_VIDEO_PAYLOAD_START_FIELD / CL_VIDEO_MIN_FIELDS_PER_CLOCK);
    constant HEADER_BEATS         : positive := (CL_VIDEO_PAYLOAD_START_FIELD / PIXELS_PER_CLOCK); 
    constant HEADER_TRIGGER_DELAY : positive := (CORE_KERNEL_LATENCY + (MAX_HEADER_BEATS - HEADER_BEATS) + 1);
    signal header_Trigger_int     : std_logic;
    signal header_Trigger_d       : std_logic_vector(1 to HEADER_TRIGGER_DELAY);
    
  begin
    
    forward_Header : process(Output_Reset, Output_Clock)
    begin
      if ?? Output_Reset then
        header_Toggle_d <= (others => '0');
        forward_State   <= FORWARD_IDLE;
      elsif rising_edge(Output_Clock) then

        -- Update delay taps
        header_Toggle_d <= (header_Toggle & header_Toggle_d(1 to (header_Toggle_d'HIGH - 1)));

        -- Update state and output assignments
        case forward_State is
          when FORWARD_ARMED =>
            -- Trigger on the indication that the decision has been made to
            -- begin the first line of the window
            if ?? window_Starting then
              forward_State <= FORWARD_IDLE;
            end if;

          when others =>
            
        end case;

        -- Detect header update events as top priority
        if ?? (header_Toggle_d(header_Toggle_d'HIGH) xor
               header_Toggle_d(header_Toggle_d'HIGH - 1)) then
          -- A new header update from the front-end; capture the header values
          -- to be forwarded, and arm the output header strobe in advance of the
          -- first window output scan.
          forward_State     <= FORWARD_ARMED;
          Header_Resolution <= latest_Resolution;
          Header_Roi_Offset <= latest_Roi_Offset;
          Header_Index      <= latest_Index;
          Header_Timestamp  <= latest_Timestamp;
        end if;

        -- Feed the delay line for triggering the header encapsulation at the
        -- appropriate time to align with multiplexed pixel data from the kernel(s)
        header_Trigger_d <= (header_Trigger_int & header_Trigger_d(1 to (header_Trigger_d'HIGH - 1)));
        
      end if;
    end process;

    -- Trigger the output header at the appropriate cycle
    header_Trigger_int <= window_Starting when (forward_State = FORWARD_ARMED) else '0';
    Header_Trigger     <= header_Trigger_d(header_Trigger_d'HIGH);

  end block;
  

  --
  -- Replication block
  --
  -- This final stage is responsible for multiplexing in either replicated plane
  -- values (nearest-neighbor-interpolated pixels) for boundary pixels, or pixel
  -- data coming from the statically-selected interpolation kernel.
  --
  replication : block

    -- Number of boundary pixels for replication
    constant BOUNDARY_PIXELS : positive := 2;

    -- Delay taps for use by the replication multiplexer
    signal window_Replicate_Row_d   : std_logic_vector(1 to CORE_KERNEL_LATENCY) := (others => '0');
    signal window_Replicate_Left_d  : std_logic_vector(1 to CORE_KERNEL_LATENCY) := (others => '0');
    signal window_Replicate_Right_d : std_logic_vector(1 to CORE_KERNEL_LATENCY) := (others => '0');
    signal steer_Replicate_Row      : std_logic;
    signal steer_Replicate_Left     : std_logic;
    signal steer_Replicate_Right    : std_logic;
    signal bayer_Replicate_Row      : std_logic;
    signal bayer_Replicate_Left     : std_logic;
    signal bayer_Replicate_Right    : std_logic;
    
  begin

    --
    -- Process managing multiplexing of replicated (nearest-neighbor) pixels
    -- and core pixels produced by the core interpolation kernel
    --
    replication_Mux : process(Output_Reset, Output_Clock)
      variable slice_High : positive;
      variable slice_Low  : natural;
    begin
      if ?? Output_Reset then
        bayer_Raw_Valid <= '0';
        bayer_Raw_Start <= '0';
        bayer_Raw_End   <= '0';
      elsif rising_edge(Output_Clock) then
        
        -- Update delay taps for making replication decisions
        window_Replicate_Row_d   <= (window_Replicate_Row & window_Replicate_Row_d(1 to (window_Replicate_Row_d'HIGH - 1)));
        window_Replicate_Left_d  <= (window_Replicate_Left & window_Replicate_Left_d(1 to (window_Replicate_Left_d'HIGH - 1)));
        window_Replicate_Right_d <= (window_Replicate_Right & window_Replicate_Right_d(1 to (window_Replicate_Right_d'HIGH - 1)));

        -- Simply forward the nearest-neighbor kernel's output stream flags;
        -- the choice doesn't matter which is used since they are latency-aligned
        bayer_Raw_Valid <= nn_Interp_Valid;
        bayer_Raw_Start <= nn_Interp_Start;
        bayer_Raw_End   <= nn_Interp_End;

        -- Clock-enable the output multiplexer
        if ?? nn_Interp_Valid then
          -- Steer in either nearest-neighbor pixels or those from the core
          -- interpolation kernel
          if ?? steer_Replicate_Row then
            -- Replicate the entire complement of pixels for the beat
            bayer_Raw_Red   <= nn_Interp_Red;
            bayer_Raw_Green <= nn_Interp_Green;
            bayer_Raw_Blue  <= nn_Interp_Blue;
          elsif ?? steer_Replicate_Left then
            -- Replicate only the first pair of pixels, packed little-endian
            --
            -- Assign the replicated boundary pixels
            slice_Low  := 0;
            slice_High := ((BOUNDARY_PIXELS * PEL_DEPTH) - 1);
            bayer_Raw_Red(slice_High downto slice_Low)   <= nn_Interp_Red(slice_High downto slice_Low);
            bayer_Raw_Green(slice_High downto slice_Low) <= nn_Interp_Green(slice_High downto slice_Low);
            bayer_Raw_Blue(slice_High downto slice_Low)  <= nn_Interp_Blue(slice_High downto slice_Low);
            
            -- Assign the core pixels far enough in from the left edge
            slice_Low  := (slice_High + 1);
            slice_High := ((PIXELS_PER_CLOCK * PEL_DEPTH) - 1);
            bayer_Raw_Red(slice_High downto slice_Low)   <= core_Interp_Red(slice_High downto slice_Low);
            bayer_Raw_Green(slice_High downto slice_Low) <= core_Interp_Green(slice_High downto slice_Low);
            bayer_Raw_Blue(slice_High downto slice_Low)  <= core_Interp_Blue(slice_High downto slice_Low);
          elsif ?? steer_Replicate_Right then
            -- Replicate only the last pair of pixels, packed little-endian
            --
            -- A corner case exists for the case of only BOUNDARY_PIXELS of datapath
            if(PIXELS_PER_CLOCK > BOUNDARY_PIXELS) then
              -- Assign the core pixels far enough in from the right edge
              slice_Low  := 0;
              slice_High := (((PIXELS_PER_CLOCK - BOUNDARY_PIXELS) * PEL_DEPTH) - 1);
              bayer_Raw_Red(slice_High downto slice_Low)   <= core_Interp_Red(slice_High downto slice_Low);
              bayer_Raw_Green(slice_High downto slice_Low) <= core_Interp_Green(slice_High downto slice_Low);
              bayer_Raw_Blue(slice_High downto slice_Low)  <= core_Interp_Blue(slice_High downto slice_Low);

              -- Only replicate the upper pixels
              slice_Low := (slice_High + 1);
            else
              -- Replicate the entire vector
              slice_Low := 0;
            end if;
              
            -- Assign the replicated boundary pixels
            slice_High := ((PIXELS_PER_CLOCK * PEL_DEPTH) - 1);
            bayer_Raw_Red(slice_High downto slice_Low)   <= nn_Interp_Red(slice_High downto slice_Low);
            bayer_Raw_Green(slice_High downto slice_Low) <= nn_Interp_Green(slice_High downto slice_Low);
            bayer_Raw_Blue(slice_High downto slice_Low)  <= nn_Interp_Blue(slice_High downto slice_Low);
          else
            -- All pixels on this beat are core pixels
            bayer_Raw_Red   <= core_Interp_Red;
            bayer_Raw_Green <= core_Interp_Green;
            bayer_Raw_Blue  <= core_Interp_Blue;
          end if;
        end if;

        -- Update debugging signals to align with the Bayer output. These are
        -- unused by any of the logic, but make interpreting line boundaries
        -- much simpler during waveform analysis
        bayer_Replicate_Row   <= steer_Replicate_Row;
        bayer_Replicate_Left  <= steer_Replicate_Left;
        bayer_Replicate_Right <= steer_Replicate_Right;
        
      end if;
    end process;

    -- Assignments to make the steering logic more readable
    steer_Replicate_Row   <= window_Replicate_Row_d(CORE_KERNEL_LATENCY);
    steer_Replicate_Left  <= window_Replicate_Left_d(CORE_KERNEL_LATENCY);
    steer_Replicate_Right <= window_Replicate_Right_d(CORE_KERNEL_LATENCY);
    
  end block;


  --
  -- Submodule implementing white-balancing of the output pixels
  --
  balancer : entity work.Bayer_Demosaic_Balance
  generic map
  (
    PEL_DEPTH        => PEL_DEPTH,
    PIXELS_PER_CLOCK => PIXELS_PER_CLOCK
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => Output_Reset,
    Pixel_Clock => Output_Clock,

    -- Interface to the register file
    White_Balance_Gains => White_Balance_Gains,

    -- Input RGB stream
    Input_Valid => bayer_Raw_Valid,
    Input_Start => bayer_Raw_Start,
    Input_End   => bayer_Raw_End,
    Input_Red   => bayer_Raw_Red,
    Input_Green => bayer_Raw_Green,
    Input_Blue  => bayer_Raw_Blue,

    -- Output RGB stream
    Output_Valid => Bayer_Valid,
    Output_Start => Bayer_Start,
    Output_End   => Bayer_End,
    Output_Red   => Bayer_Red,
    Output_Green => Bayer_Green,
    Output_Blue  => Bayer_Blue
  );


end architecture;
