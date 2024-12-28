-- File Name   : Bayer_NN_Kernel.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Nearest-neighbor interpolation kernel for the Bayer
--               demosaic core.
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


-- Architecture implementation
architecture Nearest_Neighbor of Bayer_Kernel is

  -- Constants and types for use within the kernel
  constant NN_WINDOW_SIZE   : positive := 2;
  constant BITS_PER_BEAT    : positive := (PIXELS_PER_CLOCK * PLANE_DEPTH);
  constant NUM_PIXEL_PAIRS  : positive := (PIXELS_PER_CLOCK / 2);
  subtype Window_Pels_Type is unsigned((BITS_PER_BEAT - 1) downto 0);
  type Window_Column_Type is array (0 to (NN_WINDOW_SIZE - 1)) of Window_Pels_Type;

  -- Interpolation pipeline signals
  signal window_Column : Window_Column_Type;

  -- Multiplexed versions of the input signals
  signal window_Valid_Muxed           : std_logic;
  signal window_Top_Muxed             : std_logic;
  signal window_Bottom_Muxed          : std_logic;
  signal window_Replicate_Row_Muxed   : std_logic;
  signal window_Replicate_Left_Muxed  : std_logic;
  signal window_Replicate_Right_Muxed : std_logic;
  signal window_Red_Row_Muxed         : std_logic;
  signal window_Green_First_Muxed     : std_logic;
  signal window_Column_Muxed          : Window_Column_Type;

begin

  --
  -- Begin by splitting out only a column of two sets of pixel pairs with its
  -- origin centered within the window for the NN interpolation.
  --    
  slice_Column : process(all)
    constant ORIGIN_OFFSET : natural := (((WINDOW_SIZE + 1) / 2) - 1);
    variable linear_Index  : natural;
  begin
    for row_Index in 0 to (NN_WINDOW_SIZE - 1) loop
      linear_Index := ((ORIGIN_OFFSET * WINDOW_SIZE) + row_Index + ORIGIN_OFFSET);
      window_Column(row_Index) <=
        unsigned(Window_Pixels(((Window_Pels_Type'LENGTH * (linear_Index + 1)) - 1) downto
                               (Window_Pels_Type'LENGTH * linear_Index)));
    end loop;
  end process;


  --
  -- Implement a configurable delay line for introducing extra cycles
  -- of latency to the pipeline. This may be useful for:
  --
  --   * Alignment of the output with another, parallel module
  --   * Exploiting register rebalancing for timing closure
  --
  -- Regardless of the requirement, it is best implemented here - within
  -- the kernel - rather than by client code. Client code does not have
  -- knowledge of how the input signals are used, and therefore what the
  -- most efficient way of implementing extra latency is.
  --
  -- If client code merely delayed the output, this would result in an
  -- unnecessary waste of resources, as the output has more bit depth
  -- after Bayer expansion (despite having no greater entropy).
  --
  -- In the case of this kernel, only the sliced window column and a
  -- subset of the flags need to be forwarded under delay. If the
  -- input were delayed by client code, any unused bits would certainly
  -- be eliminated during synthesis, but simulation of all the useless
  -- pixel window bits would still occur, wasting simulation time.
  --
  select_Latency : if(EXTRA_LATENCY > 0) generate

    -- Signals implementing extra delay taps
    type Window_Matrix_Type is array (natural range <>) of Window_Column_Type;
    signal window_Valid_d           : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Top_d             : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Bottom_d          : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Replicate_Row_d   : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Replicate_Left_d  : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Replicate_Right_d : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Red_Row_d         : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Green_First_d     : std_logic_vector(1 to EXTRA_LATENCY) := (others => '0');
    signal window_Column_d          : Window_Matrix_Type(1 to EXTRA_LATENCY) := (others => (others => (others => '0')));

  begin
    
    --
    -- Process systolically delaying by the needed amount
    --
    extra_Taps : process(Pixel_Clock)
    begin
      if rising_edge(Pixel_Clock) then
        window_Valid_d           <= (window_Valid & window_Valid_d(1 to (window_Valid_d'HIGH - 1)));
        window_Top_d             <= (window_Top & window_Top_d(1 to (window_Top_d'HIGH - 1)));
        window_Bottom_d          <= (window_Bottom & window_Bottom_d(1 to (window_Bottom_d'HIGH - 1)));
        window_Replicate_Row_d   <= (window_Replicate_Row & window_Replicate_Row_d(1 to (window_Replicate_Row_d'HIGH - 1)));
        window_Replicate_Left_d  <= (window_Replicate_Left & window_Replicate_Left_d(1 to (window_Replicate_Left_d'HIGH - 1)));
        window_Replicate_Right_d <= (window_Replicate_Right & window_Replicate_Right_d(1 to (window_Replicate_Right_d'HIGH - 1)));
        window_Red_Row_d         <= (window_Red_Row & window_Red_Row_d(1 to (window_Red_Row_d'HIGH - 1)));
        window_Green_First_d     <= (window_Green_First & window_Green_First_d(1 to (window_Green_First_d'HIGH - 1)));
        window_Column_d          <= (window_Column & window_Column_d(1 to (window_Column_d'HIGH - 1)));
      end if;
    end process;

    -- Assign the delayed input to the pipeline
    window_Valid_Muxed           <= window_Valid_d(EXTRA_LATENCY);
    window_Top_Muxed             <= window_Top_d(EXTRA_LATENCY);
    window_Bottom_Muxed          <= window_Bottom_d(EXTRA_LATENCY);
    window_Replicate_Row_Muxed   <= window_Replicate_Row_d(EXTRA_LATENCY);
    window_Replicate_Left_Muxed  <= window_Replicate_Left_d(EXTRA_LATENCY);
    window_Replicate_Right_Muxed <= window_Replicate_Right_d(EXTRA_LATENCY);
    window_Red_Row_Muxed         <= window_Red_Row_d(EXTRA_LATENCY);
    window_Green_First_Muxed     <= window_Green_First_d(EXTRA_LATENCY);
    window_Column_Muxed          <= window_Column_d(EXTRA_LATENCY);
    
  else generate

    -- Simply assign all the input signals straight through
    window_Valid_Muxed           <= window_Valid;
    window_Top_Muxed             <= window_Top;
    window_Bottom_Muxed          <= window_Bottom;
    window_Replicate_Row_Muxed   <= window_Replicate_Row;
    window_Replicate_Left_Muxed  <= window_Replicate_Left;
    window_Replicate_Right_Muxed <= window_Replicate_Right;
    window_Red_Row_Muxed         <= window_Red_Row;
    window_Green_First_Muxed     <= window_Green_First;
    window_Column_Muxed          <= window_Column;
    
  end generate;


  --
  -- Interpolation process
  --
  -- This process makes use of the multiplexed signals from the input stream,
  -- whether they are subjected to additional delay or not.
  --
  nn_Interpolate : process(Pixel_Clock)
    variable even_High : natural;
    variable even_Low  : natural;
    variable odd_High  : natural;
    variable odd_Low   : natural;
  begin
    if rising_edge(Pixel_Clock) then
      -- Export strobes systolically
      Output_Valid <= window_Valid_Muxed;
      Output_Start <= (window_Top_Muxed and window_Replicate_Left_Muxed);
      Output_End   <= (window_Bottom_Muxed and window_Replicate_Right_Muxed);
      
      -- Clock-enable the process upon valid window input
      if ?? window_Valid_Muxed then
        -- Loop over each pair of pixels to be interpolated
        for pair_Index in 0 to (NUM_PIXEL_PAIRS - 1) loop
          -- Offset into the output vector for each pixel within the pair
          even_Low  := (2 * pair_Index * PLANE_DEPTH);
          even_High := (even_Low + PLANE_DEPTH - 1);
          odd_Low   := (even_High + 1);
          odd_High  := (odd_Low + PLANE_DEPTH - 1);

          -- Inspect the permutation of the input color plane parities and assign
          -- the data for each pair, expanding into RGB color space.
          --
          -- Origin pixel immersion is shown from MSB .. LSB of each column of
          -- pixel pairs. If green is first on a row, it is in the low (even) pixel.
          if ?? window_Red_Row_Muxed then
            -- Red row
            if ?? window_Green_First_Muxed then
              -- Origin pixel immersed as : R G
              --                            G B
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(window_Column_Muxed(1)(even_High downto even_Low));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(window_Column_Muxed(1)(even_High downto even_Low));
            else
              -- Origin pixel immersed as : G R
              --                            B G
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(window_Column_Muxed(1)(odd_High downto odd_Low));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(window_Column_Muxed(1)(odd_High downto odd_Low));
            end if;
          else
            -- Blue row
            if ?? Window_Green_First_Muxed then
              -- Origin pixel immersed as : B G
              --                            G R
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(window_Column_Muxed(1)(even_High downto even_Low));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(window_Column_Muxed(1)(even_High downto even_Low));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
            else
              -- Origin pixel immersed as : G B
              --                            R G
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(window_Column_Muxed(1)(odd_High downto odd_Low));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(window_Column_Muxed(1)(odd_High downto odd_Low));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(window_Column_Muxed(0)(odd_High downto odd_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(window_Column_Muxed(0)(even_High downto even_Low));
            end if;
          end if;
        end loop; -- for(each input pixel pair)
      end if;
    end if;
  end process;
  
end architecture;
