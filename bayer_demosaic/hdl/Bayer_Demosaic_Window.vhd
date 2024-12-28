-- File Name   : Bayer_Demosaic_Window.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Generalized submodule for generating sliding windows of
--               input pixel data for an arbitrary set of Bayer interpolation
--               kernels.
--
--               Fundamentally, this module is a line buffer for input pixels,
--               with the ability to render the output in a distinct clock
--               domain.
--
--               The frequency of the output clock domain must be greater than
--               or equal to that of the input; if this constraint is not satisfied,
--               or marginally satisfied (i.e. with nominally-equal but distinct
--               clocks), behavior is undefined. The clocks may, however, be fully-
--               asynchronous with respect to one another.
--
--     o  0
--     | /       Copyright (c) 2017-2018
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
entity Bayer_Demosaic_Window is
  generic
  (
    -- PEL_DEPTH          - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK   - Number of horizontal pixel values per clock
    -- KERNEL_WINDOW_SIZE - Size of the window (N x N) required by the kernel
    -- MAX_HORIZ_RES      - Maximum supported horizontal resolution, in pixels
    -- START_DELAY        - Delay before starting the first line, in pixels
    PEL_DEPTH          : positive;
    PIXELS_PER_CLOCK   : positive;
    KERNEL_WINDOW_SIZE : positive;
    MAX_HORIZ_RES      : positive;
    START_DELAY        : positive
  );
  port
  (
    -- Input reset and clock
    Input_Reset : in std_logic;
    Input_Clock : in std_logic;

    -- Interface to the register file
    Row_Mode    : in Row_Mode_Type;
    Column_Mode : in Column_Mode_Type;

    -- Input resolution and pixel stream
    Input_Resolution : in CL_Video_Coordinate;
    Input_Valid      : in std_logic;
    Input_Start      : in std_logic;
    Input_End        : in std_logic;
    Input_Pixels     : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    
    -- Output reset and clock
    Output_Reset : in std_logic;
    Output_Clock : in std_logic;

    -- Ready signal from the output interface
    Output_Ready : in std_logic;

    -- Strobe indicating when the window is starting a new frame
    Output_Starting : out std_logic;

    -- Generated output window interface
    --
    -- The raw pixel values are packed into a vector to permit the use
    -- of a generic value to derive the window size. If VHDL-2008 package
    -- generics were supported, this could be a nicer, constrained matrix.
    Output_Valid           : out std_logic := '0';
    Output_Top             : out std_logic := '0';
    Output_Bottom          : out std_logic := '0';
    Output_Replicate_Row   : out std_logic := '0';
    Output_Replicate_Left  : out std_logic := '0';
    Output_Replicate_Right : out std_logic := '0';
    Output_Red_Row         : out std_logic := '0';
    Output_Green_First     : out std_logic := '0';
    Output_Pixels          : out std_logic_vector((((KERNEL_WINDOW_SIZE ** 2) * PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0)
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Demosaic_Window is
    
  -- Input coordinate generator signals
  signal input_Coord_Valid : std_logic;
  signal input_Coordinate  : CL_Video_Coordinate;
  signal input_Vert_First  : std_logic;
  signal input_Vert_Last   : std_logic;
  signal input_Horiz_First : std_logic;
  signal input_Horiz_Last  : std_logic;

  -- Constants and types for interacting with the line buffers
  constant NUM_BUFFER_LINES  : positive := (KERNEL_WINDOW_SIZE + 1);
  constant BUFFER_INDEX_BITS : positive := Min_Repr_Bits(NUM_BUFFER_LINES - 1);
  subtype Buffer_Index_Type is unsigned((BUFFER_INDEX_BITS - 1) downto 0);
  constant PIXEL_LANE_INDEX_BITS : positive := Min_Repr_Bits(PIXELS_PER_CLOCK - 1);
  constant MAX_HORIZ_RES_BITS    : positive := Min_Repr_Bits(MAX_HORIZ_RES - 1);
  constant BEAT_ADDRESS_WIDTH    : positive := (MAX_HORIZ_RES_BITS - PIXEL_LANE_INDEX_BITS);
  subtype Buffer_Line_Beat_Address is unsigned((BEAT_ADDRESS_WIDTH - 1) downto 0);
  constant MAX_LINE_BEATS : positive := (2 ** BEAT_ADDRESS_WIDTH);
  subtype Pixel_Word_Type is std_logic_vector(Input_Pixels'RANGE);
  type Pixel_Column_Type is array (0 to (KERNEL_WINDOW_SIZE - 1)) of Pixel_Word_Type;
  type Pixel_Matrix_Type is array (0 to (KERNEL_WINDOW_SIZE - 1)) of Pixel_Column_Type;
  
  -- Signals for the line buffers
  signal write_Buffer_Enable : std_logic;
  signal write_Buffer_Index  : Buffer_Index_Type;
  signal write_Buffer_Beat   : Buffer_Line_Beat_Address;
  signal write_Buffer_First  : std_logic;
  signal write_Buffer_Last   : std_logic;
  signal write_Buffer_Data   : Pixel_Word_Type;
  signal read_Buffer_Enable  : std_logic;
  signal read_Buffer_Index   : Buffer_Index_Type;
  signal read_Buffer_Beat    : Buffer_Line_Beat_Address;
  signal read_Buffer_Data    : std_logic_vector(((KERNEL_WINDOW_SIZE * Pixel_Word_Type'LENGTH) - 1) downto 0);
  signal read_Buffer_Column  : Pixel_Column_Type;

  -- Signals for triggering the read process
  constant SYNC_TOGGLE_TAPS  : positive := 6;
  signal write_Line_Toggle   : std_logic;
  signal write_Line_Index    : Buffer_Index_Type;
  signal write_Line_First    : std_logic;
  signal write_Line_Last     : std_logic;
  signal write_Line_Toggle_d : std_logic_vector(1 to SYNC_TOGGLE_TAPS);
  signal window_Armed        : std_logic;
  signal window_Index        : Buffer_Index_Type;
  signal window_First        : std_logic;
  signal window_Last         : std_logic;
  signal window_Ack          : std_logic;
  signal drain_Armed         : std_logic;
      
begin

  --
  -- Coordinate generator used to produce write addresses and detect
  -- input framing events for the window generation logic
  --
  input_Tracker : entity work.Bayer_Demosaic_Coords
  generic map
  (
    PIXELS_PER_CLOCK => PIXELS_PER_CLOCK
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => Input_Reset,
    Pixel_Clock => Input_Clock,

    -- Declared input resolution
    Input_Resolution => Input_Resolution,

    -- Input pixel stream control
    Input_Valid => Input_Valid,
    Input_Start => Input_Start,
    
    -- Output coordinate and replication interface
    Coord_Valid     => input_Coord_Valid,
    Coordinate      => input_Coordinate,
    Vert_First      => input_Vert_First,
    Vert_Last       => input_Vert_Last,
    Horiz_First     => input_Horiz_First,
    Horiz_Last      => input_Horiz_Last,
    Replicate_Row   => open,
    Replicate_Left  => open,
    Replicate_Right => open
  );


  --
  -- Line buffer write control
  --
  -- This block manages the writing of input pixel data to the set of line buffers
  -- used by the output window generator logic.
  --
  buffer_Writes : block
  begin

    -- Enable write data as it is presented to the pipeline
    write_Buffer_Enable <= Input_Valid when rising_edge(Input_Clock);

    -- Capture the write data into a pipeline tap
    write_Buffer_Data <= Input_Pixels when (Input_Valid  = '1') and rising_edge(Input_Clock);

    -- State machine writing into the line buffers
    write_Logic : process(Input_Reset, Input_Clock)
      
    begin
      if ?? Input_Reset then
        -- Reset the initial write buffer index; its count can never be
        -- terminal, as a minimum of two buffers are required for any Bayer
        -- pattern demosaic across all three RGB color planes.
        write_Buffer_Index <= To_Unsigned(0, write_Buffer_Index'LENGTH);
        write_Line_Toggle  <= '0';
      elsif rising_edge(Input_Clock) then
        -- Clock-enable most of the process on the valid strobe
        if ?? Input_Valid then
          -- Synchronously reset state on the first beat of each frame
          if ?? Input_Start then
            -- Advance to the first beat of the the next write buffer index
            write_Buffer_Index <= ((write_Buffer_Index + 1) mod NUM_BUFFER_LINES);

            -- Indicate the first buffer of the incoming frame
            write_Buffer_First <= '1';
            write_Buffer_Last  <= '0';
            write_Buffer_Beat  <= To_Unsigned(0, write_Buffer_Beat'LENGTH);
          else
            -- Look for the last pixel of a line
            if ?? input_Horiz_Last then
              -- Advance line buffers on horizontal last, and no longer flag the
              -- first write buffer of a frame
              write_Buffer_Index <= ((write_Buffer_Index + 1) mod NUM_BUFFER_LINES);
              write_Buffer_First <= '0';
              write_Buffer_Last  <= input_Vert_Last;
              write_Buffer_Beat  <= To_Unsigned(0, write_Buffer_Beat'LENGTH);
            else
              write_Buffer_Beat <= (write_Buffer_Beat + 1);
            end if;

          end if;
        end if; -- if(input valid)

        -- Toggle the "line written" indication to the output domain as the last
        -- beat of each line is written
        if ?? (write_Buffer_Enable and input_Horiz_Last) then
          -- Capture the present line buffer and "first line" flag for the output
          write_Line_Toggle <= not write_Line_Toggle;
          write_Line_Index  <= write_Buffer_Index;
          write_Line_First  <= write_Buffer_First;
          write_Line_Last   <= write_Buffer_Last;
        end if;

      end if;
    end process;

  end block;


  --
  -- Line buffering for window generation
  line_Buffers : entity work.Bayer_Demosaic_Buffers
  generic map
  (
    NUM_LINES          => NUM_BUFFER_LINES,
    LINE_INDEX_BITS    => BUFFER_INDEX_BITS,
    BEAT_ADDRESS_WIDTH => BEAT_ADDRESS_WIDTH,
    PIXEL_BEAT_WIDTH   => Input_Pixels'LENGTH
  )
  port map
  (
    -- Write interface
    Write_Clock   => Input_Clock,
    Write_Enable  => write_Buffer_Enable,
    Write_Index   => write_Buffer_Index,
    Write_Address => write_Buffer_Beat,
    Write_Data    => write_Buffer_Data,

    -- Read interface
    Read_Clock   => Output_Clock,
    Read_Enable  => read_Buffer_Enable,
    Read_Index   => read_Buffer_Index,
    Read_Address => read_Buffer_Beat,
    Read_Data    => read_Buffer_Data
  );


  --
  -- Split the raw read buffer data into a columnar array for the window
  --
  -- NOTE - This would be unnecessary if VHDL-2008 package generics *or*
  --        unconstrained array types were supported; the requisite array
  --        type could be defined for the full scope of an instance in
  --        either case to avoid this Verilog-esque packing / unpacking.
  separate_Column : process(all)
  begin
    
    -- Split the raw data vector by rows
    for row_Index in 0 to (KERNEL_WINDOW_SIZE - 1) loop
      read_Buffer_Column(row_Index) <=
        read_Buffer_Data((((row_Index + 1) * Pixel_Word_Type'LENGTH) - 1) downto
                         (row_Index * Pixel_Word_Type'LENGTH));
    end loop;
  
  end process;


  --
  -- Buffer read logic, feeding the window generator output
  --
  buffer_Reads : block

    -- State space for window generation
    type Window_State_Type is (WINDOW_IDLE, WINDOW_DELAY, WINDOW_ACTIVE, WINDOW_DRAIN);
    signal next_Window_State    : Window_State_Type;
    signal present_Window_State : Window_State_Type;
    constant DELAY_COUNT_BITS   : positive := Min_Repr_Bits(START_DELAY - 1);
    signal delay_Counter        : unsigned((DELAY_COUNT_BITS - 1) downto 0) := To_Unsigned(0, DELAY_COUNT_BITS);
    signal delay_Counter_Reset  : std_logic;
    signal delay_Counter_Tc     : std_logic;
    signal window_Valid         : std_logic;
    signal window_Start         : std_logic;
    signal output_Ready_d       : std_logic;
    
    -- Window coordinate generator signals
    signal window_Coord_Valid     : std_logic;
    signal window_Coordinate      : CL_Video_Coordinate;
    signal window_Vert_First      : std_logic;
    signal window_Vert_Last       : std_logic;
    signal window_Horiz_First     : std_logic;
    signal window_Horiz_Last      : std_logic;
    signal window_Replicate_Row   : std_logic;
    signal window_Replicate_Left  : std_logic;
    signal window_Replicate_Right : std_logic;
    signal first_Line_Armed       : std_logic;
    signal first_Line_Armed_d     : std_logic;
    signal window_Ack_d           : std_logic;
    signal window_Top             : std_logic;
    signal window_Bottom          : std_logic;

    -- Signals for read-side interfacing to the buffer RAMs
    constant BUFFER_READ_LATENCY    : positive := 4;
    signal window_Coord_Valid_d     : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Top_d             : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Bottom_d          : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Replicate_Row_d   : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Replicate_Left_d  : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Replicate_Right_d : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Parity_Horiz_d    : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    signal window_Parity_Vert_d     : std_logic_vector(1 to BUFFER_READ_LATENCY) := (others => '0');
    
    -- Signals for the pixel window matrix
    signal advance_Matrix         : std_logic;
    signal matrix_Data_Advanced   : std_logic := '0';
    signal matrix_Last_Data       : std_logic := '0';
    signal matrix_Top             : std_logic := '0';
    signal matrix_Bottom          : std_logic := '0';
    signal matrix_Replicate_Row   : std_logic := '0';
    signal matrix_Replicate_Left  : std_logic := '0';
    signal matrix_Replicate_Right : std_logic := '0';
    signal matrix_Horiz_Parity    : std_logic := '0';
    signal matrix_Vert_Parity     : std_logic := '0';
    signal matrix_Red_Row         : std_logic := '0';
    signal matrix_Green_First     : std_logic := '0';
    signal pixel_Matrix           : Pixel_Matrix_Type := (others => (others => (others => '0')));
    
  begin

    -- Process responding to buffer write activity, generating sliding windows
    -- of pixel data for interpolation
    arm_Window : process(Output_Reset, Output_Clock)
    begin
      if ?? Output_Reset then
        write_Line_Toggle_d <= (others => '0');
        window_Armed        <= '0';
      elsif rising_edge(Output_Clock) then
        
        -- Update delay taps
        write_Line_Toggle_d <= (write_Line_Toggle &
                                write_Line_Toggle_d(1 to (write_Line_Toggle_d'HIGH - 1)));

        -- Detect end-of-line-written events occurring in the write domain
        if ?? (write_Line_Toggle_d(write_Line_Toggle_d'HIGH) xor
               write_Line_Toggle_d(write_Line_Toggle_d'HIGH - 1)) then
          -- Arm the window generation for the next line (some overlap may occur
          -- when the input and output clocks are the same or nearly the same),
          -- and capture the starting buffer index and first-line flag for the
          -- upcoming sliding window generation.
          window_Armed <= '1';
          window_Index <= write_Line_Index;
          window_First <= write_Line_First;
          window_Last  <= write_Line_Last;
        elsif ?? window_Ack then
          window_Armed <= '0';
        end if;
        
      end if;
    end process;


    --
    -- Window generation state machine
    --
    window_Gen_comb : process(all)
    begin
      
      -- Tend to remain in the present state
      next_Window_State <= present_Window_State;

      -- Default Mealy assignments
      window_Ack          <= '0';
      window_Valid        <= '0';
      delay_Counter_Reset <= '1';

      -- Next-state excitation logic
      case present_Window_State is
        when WINDOW_IDLE =>
          -- Go active from this state when the window is armed by the input and
          -- the output is ready for data
          if ?? (window_Armed and output_Ready_d) then
            -- Skip starting for the first line
            if not window_First then
              -- Induce a short delay on the first line
              if ?? first_Line_Armed then
                next_Window_State <= WINDOW_DELAY;
              else
                next_Window_State <= WINDOW_ACTIVE;
              end if;
            end if;

            -- Acknowledge the arming in either case
            window_Ack <= '1';
          end if;
          

        when WINDOW_DELAY =>
          -- Permit the delay counter to advance to its terminal count
          delay_Counter_Reset <= '0';
          if ?? delay_Counter_Tc then
            next_Window_State <= WINDOW_ACTIVE;
          end if;

          
        when WINDOW_ACTIVE =>
          -- Test for the end-of-line condition
          if ?? (window_Coord_Valid and window_Horiz_Last) then
            -- End of a line; continue onto the next line and acknowledge
            -- if we are already re-armed
            window_Ack <= (window_Armed or drain_Armed);
            if ?? (window_Armed and not window_First) then
              -- Continue along to the next line unconditionally; we must
              -- respond within this valid-coordinate cycle or it will be lost.
              window_Valid <= '1';
            else
              -- This is the end of a line, and either the first line or no
              -- line is armed. Determine whether to drain the final line or
              -- go idle.
              if ?? drain_Armed then
                -- Continue to the drain line unconditionally
                window_Valid      <= '1';
                next_Window_State <= WINDOW_DRAIN;
              else
                -- Simply nothing to do on this cycle, go idle. Idle cycles during
                -- a frame serve to absorb input idle cycles and any input / output
                -- clock frequency difference.
                next_Window_State <= WINDOW_IDLE;
              end if;
            end if; -- if(starting armed line)
          else
            -- During most of each line, modulate the valid signal. This moves
            -- the coordinate generator logic forward, using the pipeline-delayed
            -- version of the output ready signal.
            --
            -- Downstream logic absorbs the inertia of the pipeline as a whole.
            window_Valid <= output_Ready_d;
          end if;
          

        when WINDOW_DRAIN =>
          -- Drain the final line, continuing to respect the ready signal at
          -- the source of the window generation activity
          if ?? (window_Coord_Valid and window_Horiz_Last) then
            -- This is the final pixel of the frame; either go idle. There is always
            -- at least one idle cycle to accommodate the header word, even in
            -- back-to-back video packet conditions.
            next_Window_State <= WINDOW_IDLE;
          else
            -- It is anticipated that the first line of the next frame may become
            -- armed during this line. Clear this condition as it occurs, but
            -- do not clear the first line if it arms.
            window_Ack <= (window_Armed and window_First);
              
            -- Continue to modulate the window progress in respect of backpressure
            window_Valid <= output_Ready_d;
          end if;
          
      end case;

    end process;

    window_Gen_seq : process(Output_Reset, Output_Clock)
    begin
      if ?? Output_Reset then
        present_Window_State <= WINDOW_IDLE;
        first_Line_Armed     <= '0';
        window_Start         <= '1';
        output_Ready_d       <= '0';
        read_Buffer_Index    <= To_Unsigned(0, read_Buffer_Index'LENGTH);
        drain_Armed          <= '0';
        window_Top           <= '0';
        window_Bottom        <= '0';
        delay_Counter        <= To_Unsigned(0, delay_Counter'LENGTH);
        Output_Starting      <= '0';
      elsif rising_edge(Output_Clock) then
        
        -- Advance to the next state
        present_Window_State <= next_Window_State;

        -- Default Moore assignments
        window_Start    <= '0';
        Output_Starting <= '0'; 

        -- Update delay taps
        output_Ready_d <= Output_Ready;
        window_Ack_d   <= window_Ack;

        -- Run the delay counter on command
        if ?? delay_Counter_Reset then
          delay_Counter <= To_Unsigned(0, delay_Counter'LENGTH);
        elsif not delay_Counter_Tc then
          delay_Counter <= (delay_Counter + 1);
        end if;
        
        -- Output excitation logic
        case present_Window_State is
          when WINDOW_IDLE =>
            -- The first cycle out of idle may be the start of frame; capture
            -- the starting buffer index for the window reads
            if ?? window_Ack then
              -- Latch the first / last flags for the next stage
              first_Line_Armed   <= window_First;
              first_Line_Armed_d <= first_Line_Armed;
            
              -- Capture the window starting line index
              read_Buffer_Index <= window_Index;

              -- The bottom (drain) line never starts from this state
              window_Bottom <= '0';
            end if;

            -- Strobe the decision to start the first line of the output window
            if(next_Window_State = WINDOW_DELAY) then
              Output_Starting <= '1';
            end if;


          when WINDOW_DELAY =>
            -- Wait for the terminal count of the delay counter, then indicate
            -- the window starting cycle as we go active
            window_Start <= delay_Counter_Tc;

            
          when WINDOW_ACTIVE =>
            -- Update the window starting line index when moving directly to
            -- the next line
            if ?? window_Ack then
              -- Also update the "first line armed" state bit, as we may or may
              -- not be actually continuing along to this next line
              first_Line_Armed   <= window_First;
              first_Line_Armed_d <= first_Line_Armed;
            
              -- Rotate the index unconditionally, locally rotating on the final
              -- line, which occurs automatically without an input line write
              if ?? drain_Armed then
                read_Buffer_Index <= ((window_Index + 1) mod NUM_BUFFER_LINES);
              else
                read_Buffer_Index <= window_Index;
              end if;
            end if;

            if ?? (window_Valid and (window_Start or window_Ack)) then
              window_Top <= window_Start;
            end if; -- if(window valid)

            if(next_Window_State = WINDOW_DRAIN) then
              window_Bottom <= '1';
            end if;
            
          when WINDOW_DRAIN =>

        end case;

        -- Arm the drain process synchronously for the next-to-final line
        if ?? window_Coord_Valid then
          drain_Armed <= '0';
          if(window_Coordinate.y = (Input_Resolution.y - 2)) then
            drain_Armed <= '1';
          end if;
        end if;

      end if;
    end process;

    -- Combinatorially detect terminal delay count
    delay_Counter_Tc <= '1' when (delay_Counter >= (START_DELAY - 1)) else '0';


    --
    -- Coordinate generator for output window tracking
    --
    window_Tracker : entity work.Bayer_Demosaic_Coords
    generic map
    (
      PIXELS_PER_CLOCK => PIXELS_PER_CLOCK
    )
    port map
    (
      -- Pixel reset and clock
      Pixel_Reset => Output_Reset,
      Pixel_Clock => Output_Clock,
    
      -- Declared input resolution
      Input_Resolution => Input_Resolution,
    
      -- Input pixel stream control
      Input_Valid => window_Valid,
      Input_Start => window_Start,
      
      -- Output coordinate and replication interface
      Coord_Valid     => window_Coord_Valid,
      Coordinate      => window_Coordinate,
      Vert_First      => window_Vert_First,
      Vert_Last       => window_Vert_Last,
      Horiz_First     => window_Horiz_First,
      Horiz_Last      => window_Horiz_Last,
      Replicate_Row   => window_Replicate_Row,
      Replicate_Left  => window_Replicate_Left,
      Replicate_Right => window_Replicate_Right
    );

    
    -- Update delay lines from the first stage
    pipe_Delays : process(Output_Clock)
    begin
      if rising_edge(Output_Clock) then
        -- Govern downstream logic by qualification with systolically-
        -- delayed versions of the "window valid" signal from the window
        -- coordinate tracking module.
        window_Coord_Valid_d <= (window_Coord_Valid &
                                 window_Coord_Valid_d(1 to (window_Coord_Valid_d'HIGH - 1)));

        -- Delay other window state variables to align with their active cycles
        window_Top_d             <= (window_Top & window_Top_d(1 to (window_Top_d'HIGH - 1)));
        window_Bottom_d          <= (window_Bottom & window_Bottom_d(1 to (window_Bottom_d'HIGH - 1)));
        window_Replicate_Row_d   <= (window_Replicate_Row & window_Replicate_Row_d(1 to (window_Replicate_Row_d'HIGH - 1)));
        window_Replicate_Left_d  <= (window_Replicate_Left & window_Replicate_Left_d(1 to (window_Replicate_Left_d'HIGH - 1)));
        window_Replicate_Right_d <= (window_Replicate_Right & window_Replicate_Right_d(1 to (window_Replicate_Right_d'HIGH - 1)));

        -- Identify the even / odd parity for both coordinates
        --
        -- The horizontal coordinate counts by pixels groups, so the corresponding
        -- coordinate bit is sliced.
        window_Parity_Horiz_d <= (window_Coordinate.x(PIXEL_LANE_INDEX_BITS) & window_Parity_Horiz_d(1 to (window_Parity_Horiz_d'HIGH - 1)));
        window_Parity_Vert_d  <= (window_Coordinate.y(0) & window_Parity_Vert_d(1 to (window_Parity_Vert_d'HIGH - 1)));
      end if;
    end process;

    -- Enable the read interface as new addresses are presented, and for
    -- long enough to drain its final data
    read_Buffer_Enable <= (window_Coord_Valid or (or window_Coord_Valid_d));    

    -- Drive the read beat address from the significant group of horizontal
    -- window coordinate bits
    read_Buffer_Beat <= window_Coordinate.x((read_Buffer_Beat'HIGH + PIXEL_LANE_INDEX_BITS) downto
                                            PIXEL_LANE_INDEX_BITS);


    --
    -- Process collecting the read column into a coherent, properly-
    -- framed window for presentation to the interpolation kernel(s)
    --
    collect_Window : process(Output_Clock)
    begin
      if rising_edge(Output_Clock) then
        -- Examine the last delay taps from the window excitation and coordinate
        -- tracking stage. The window is run for one final clock after the last tap.
        matrix_Data_Advanced <= window_Coord_Valid_d(window_Coord_Valid_d'HIGH);
        
        if ?? advance_Matrix then
          -- Advance qualifing flag delay line taps to their respective outputs
          matrix_Top             <= window_Top_d(window_Top_d'HIGH);
          matrix_Bottom          <= window_Bottom_d(window_Bottom_d'HIGH);
          matrix_Replicate_Row   <= window_Replicate_Row_d(window_Replicate_Row_d'HIGH);
          matrix_Replicate_Left  <= window_Replicate_Left_d(window_Replicate_Left_d'HIGH);
          matrix_Replicate_Right <= window_Replicate_Right_d(window_Replicate_Right_d'HIGH);
          matrix_Horiz_Parity    <= window_Parity_Horiz_d(window_Parity_Horiz_d'HIGH);
          matrix_Vert_Parity     <= window_Parity_Vert_d(window_Parity_Vert_d'HIGH);

          -- Capture whether this was the last data advance of a frame in order
          -- to extend the advance for one extra cycle, compensating for the fact
          -- that the first read cycle is ignored for purposes of immersion.
          matrix_Last_Data <= (window_Bottom_d(window_Bottom_d'HIGH) and
                               window_Replicate_Right_d(window_Replicate_Right_d'HIGH));
          
        end if;

        -- Mark the window as valid once the minimum immersion for nearest-neighbor
        -- replication has been satisfied
        Output_Valid <= '0';
        if ?? matrix_Data_Advanced then
          -- Advance the matrix of pixels, ingesting new columns as they arrive.
          -- Pixels are indexed in column-major fashion, as in MATLAB / NumPy.
          Output_Valid <= '1';
          
          -- Drive out the qualifier signals whenever they have been advanced
          Output_Top             <= matrix_Top;
          Output_Bottom          <= matrix_Bottom;
          Output_Replicate_Row   <= matrix_Replicate_Row;
          Output_Replicate_Left  <= matrix_Replicate_Left;
          Output_Replicate_Right <= matrix_Replicate_Right;
          Output_Red_Row         <= matrix_Red_Row;
          Output_Green_First     <= matrix_Green_First;
        end if;

        -- Advance the data pipeline on either of the two phases effectively
        -- created by the single-cycle immersion delay
        if ?? (advance_Matrix or matrix_Data_Advanced) then
          for row_Index in 0 to (KERNEL_WINDOW_SIZE - 1) loop
            pixel_Matrix(pixel_Matrix'HIGH)(row_Index) <= read_Buffer_Column(row_Index);
          end loop;

          for column_Index in 0 to (KERNEL_WINDOW_SIZE - 2) loop
            for row_Index in 0 to (KERNEL_WINDOW_SIZE - 1) loop
              pixel_Matrix(column_Index)(row_Index) <=
                pixel_Matrix(column_Index + 1)(row_Index);
            end loop;
          end loop;
        end if;
      end if;
    end process;

    -- Advance the pixel matrix whenever new columnar data arrives or for
    -- one more beat after the last column of a line has been advanced
    advance_Matrix <= (window_Coord_Valid_d(window_Coord_Valid_d'HIGH) or
                       (matrix_Data_Advanced and matrix_Last_Data));


    -- Combine the horizontal and vertical parity with the Bayer pattern
    -- column and row mode register settings to determine the pattern
    -- location signals
    matrix_Red_Row     <= matrix_Vert_Parity when (Row_Mode = BLUE_FIRST) else
                          not matrix_Vert_Parity;
    
    matrix_Green_First <= matrix_Vert_Parity when (Column_Mode = GREEN_SECOND) else
                          not matrix_Vert_Parity;
    
      
    --
    -- Pack the pixel window into a flat vector of bits for export
    -- to the downstream interpolation kernel(s). This could be passed
    -- directly as a package-declared array type if VHDL-2008 package
    -- generics were supported...
    --
    pack_Window : process(all)
      variable linear_Index : natural;
    begin
      -- Pack the pixels into the vector in column-major order, as in MATLAB / NumPy
      for column_Index in 0 to (KERNEL_WINDOW_SIZE - 1) loop
        for row_Index in 0 to (KERNEL_WINDOW_SIZE - 1) loop
          linear_Index := ((column_Index * KERNEL_WINDOW_SIZE) + row_Index);
          Output_Pixels(((Pixel_Word_Type'LENGTH * (linear_Index + 1)) - 1) downto
                        (Pixel_Word_Type'LENGTH * linear_Index)) <=
            pixel_Matrix(column_index)(row_Index);
        end loop;
      end loop;
    end process;
    
  end block;
  
end architecture;
