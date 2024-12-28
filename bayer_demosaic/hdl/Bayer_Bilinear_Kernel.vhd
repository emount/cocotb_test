-- File Name   : Bayer_Bilinear_Kernel.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Bi-linear interpolation kernel for the Bayer demosaic core.
--
--     o  0
--     | /       Copyright (c) 2018
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
architecture Bilinear of Bayer_Kernel is

  -- Locally capture constants from the Bayer demosaic core's primary
  -- implementation-specific package and the kernel definitions package.
  -- This ensures both remain in sync with the actual implementation herein.
  -- constant WINDOW_SIZE    : positive := Bayer_Kernel_Window_Size(BAYER_INTERP_BILINEAR);
  constant KERNEL_LATENCY : positive := Bayer_Kernel_Latency(BAYER_INTERP_BILINEAR);
  
  -- Other constants and types for use within the kernel
  constant BITS_PER_BEAT   : positive := (PIXELS_PER_CLOCK * PLANE_DEPTH);
  constant NUM_PIXEL_PAIRS : positive := (PIXELS_PER_CLOCK / 2);
  subtype Window_Pels_Type is std_logic_vector((BITS_PER_BEAT - 1) downto 0);
  type Window_Vector_Type is array (natural range <>) of Window_Pels_Type;
  type Window_Matrix_Type is array (0 to (WINDOW_SIZE - 1)) of Window_Vector_Type(0 to (WINDOW_SIZE - 1));

  -- Constants and types for feeding the interpolator primitives
  constant PEL_A : natural := 0;
  constant PEL_B : natural := 1;
  constant PEL_C : natural := 2;
  constant PEL_D : natural := 3;
  subtype Pel_Type is unsigned((PLANE_DEPTH - 1) downto 0);
  type Pel_Pair_Vector is array (0 to (NUM_PIXEL_PAIRS - 1)) of Pel_Type;
  type Pel_Pair_Matrix is array (natural range <>) of Pel_Pair_Vector;

  -- Delay taps for straight-through forwarded pel values
  constant INTERP_LATENCY : positive := 2;
  signal copy_Pels        : Window_Pels_Type;
  signal copy_Pels_d      : Window_Vector_Type(1 to INTERP_LATENCY);

  -- Interpolator stage operands and output pels
  signal green_Interp4_Ops     : Pel_Pair_Matrix(PEL_A to PEL_D);
  signal red_Blue_Interp4_Ops  : Pel_Pair_Matrix(PEL_A to PEL_D);
  signal red_Blue_Interp2X_Ops : Pel_Pair_Matrix(PEL_A to PEL_B);
  signal red_Blue_Interp2Y_Ops : Pel_Pair_Matrix(PEL_A to PEL_B);
  signal green_Interp4         : Pel_Pair_Vector;
  signal red_Blue_Interp4      : Pel_Pair_Vector;
  signal red_Blue_Interp2X     : Pel_Pair_Vector;
  signal red_Blue_Interp2Y     : Pel_Pair_Vector;

  -- Interpolation pipeline signals
  signal window_Matrix            : Window_Matrix_Type := (others => (others => (others => '0')));
  signal window_Start             : std_logic;
  signal window_End               : std_logic;
  signal window_Valid_d           : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Top_d             : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Bottom_d          : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Replicate_Row_d   : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Replicate_Left_d  : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Replicate_Right_d : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Red_Row_d         : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Green_First_d     : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_Start_d           : std_logic_vector(1 to (KERNEL_LATENCY - 1));
  signal window_End_d             : std_logic_vector(1 to (KERNEL_LATENCY - 1));
      
begin

  --
  -- Begin by unpacking the window vector into its source matrix. This could be passed
  -- directly as a package-declared array type if VHDL-2008 package
  -- generics were supported...
  --
  unpack_Window : process(all)
    variable linear_Index : natural;
  begin
    -- Pack the pixels into the vector in column-major order, as in MATLAB / NumPy
    for column_Index in 0 to (WINDOW_SIZE - 1) loop
      for row_Index in 0 to (WINDOW_SIZE - 1) loop
        linear_Index := ((column_Index * WINDOW_SIZE) + row_Index);
        window_Matrix(column_index)(row_Index) <=
          Window_Pixels(((Window_Pels_Type'LENGTH * (linear_Index + 1)) - 1) downto
                        (Window_Pels_Type'LENGTH * linear_Index));
      end loop;
    end loop;
  end process;
   
   
  --
  -- Interpolation process
  --
  bilinear_Interpolate : process(Pixel_Clock)
    variable even_High : natural;
    variable even_Low  : natural;
    variable odd_High  : natural;
    variable odd_Low   : natural;
  begin
    if rising_edge(Pixel_Clock) then
      -- Run all qualifying signals through systolic delay lines for use
      -- in each stage of the interpolation pipeline
      window_Valid_d           <= (window_Valid & window_Valid_d(1 to (window_Valid_d'HIGH - 1)));
      window_Top_d             <= (window_Top & window_Top_d(1 to (window_Top_d'HIGH - 1)));
      window_Bottom_d          <= (window_Bottom & window_Bottom_d(1 to (window_Bottom_d'HIGH - 1)));
      window_Replicate_Row_d   <= (window_Replicate_Row & window_Replicate_Row_d(1 to (window_Replicate_Row_d'HIGH - 1)));
      window_Replicate_Left_d  <= (window_Replicate_Left & window_Replicate_Left_d(1 to (window_Replicate_Left_d'HIGH - 1)));
      window_Replicate_Right_d <= (window_Replicate_Right & window_Replicate_Right_d(1 to (window_Replicate_Right_d'HIGH - 1)));
      window_Red_Row_d         <= (window_Red_Row & window_Red_Row_d(1 to (window_Red_Row_d'HIGH - 1)));
      window_Green_First_d     <= (window_Green_First & window_Green_First_d(1 to (window_Green_First_d'HIGH - 1)));
      window_Start_d           <= (window_Start & window_Start_d(1 to (window_Start_d'HIGH - 1)));
      window_End_d             <= (window_End & window_End_d(1 to (window_End_d'HIGH - 1)));
      
      -- Clock-enable the process upon valid window input
      if ?? Window_Valid then
        -- Loop over each pair of pixels to be interpolated
        for pair_Index in 0 to (NUM_PIXEL_PAIRS - 1) loop
          -- Offset into the output vector for each pixel within the pair
          even_Low  := (2 * pair_Index * PLANE_DEPTH);
          even_High := (even_Low + PLANE_DEPTH - 1);
          odd_Low   := (even_High + 1);
          odd_High  := (odd_Low + PLANE_DEPTH - 1);

          -- Each pair of input pels is copied-through for multiplexing to the
          -- appropriate color plane at the output stage
          copy_Pels(odd_High downto even_Low) <= window_Matrix(1)(1)(odd_High downto even_Low);
          
          -- The assignment of operand pels to their respective interpolation primitives
          -- depends solely upon whether this row has the green pel first or second.
          -- 
          -- Origin pixel immersion is shown from MSB .. LSB of each column of
          -- pixel pairs. If green is first on a row, it is in the low (even) pixel.
          -- Red and blue pels are labeled 'X' and 'Y' interchangeably; their row-major
          -- parity does not affect operand assignment, only output multiplexing.
          --
          -- Pel operand labeling for four-pel interpolation:
          --
          --    A         A   B
          --  B o C   or    o
          --    D         C   D
          --
          -- Pel operand labeling for two-pel interpolation:
          --
          --    A
          --    o   or   A o B
          --    B
          if ?? Window_Green_First then
            -- Origin pixel pair immersed as : Column 1 | Column 0
            --                                 ---------|---------
            --                                    X G X | G
            --                                    G Y G | Y
            --                                    X G X | G
            --
            -- The 4-interpolated green pel goes to the odd pair member
            green_Interp4_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)(odd_High downto odd_Low));
            if(pair_Index = (NUM_PIXEL_PAIRS - 1)) then
              -- Obtain from the lowest-order pel in column two
              green_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(2)(1)((PLANE_DEPTH - 1) downto 0));
            else
              -- Obtain from the next higher-order even pel
              green_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(1)((odd_High + PLANE_DEPTH) downto (odd_Low + PLANE_DEPTH)));
            end if;
            green_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(1)(1)(even_High downto even_Low));
            green_Interp4_Ops(PEL_D)(pair_Index) <= unsigned(window_Matrix(1)(2)(odd_High downto odd_Low));

            -- The 4-interpolated red / blue pel goes to the odd pair member as well
            if(pair_Index = (NUM_PIXEL_PAIRS - 1)) then
              red_Blue_Interp4_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(2)(0)((PLANE_DEPTH - 1) downto 0));
              red_Blue_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(2)(2)((PLANE_DEPTH - 1) downto 0));
            else
              red_Blue_Interp4_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)((odd_High + PLANE_DEPTH) downto (odd_Low + PLANE_DEPTH)));
              red_Blue_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(1)(2)((odd_High + PLANE_DEPTH) downto (odd_Low + PLANE_DEPTH)));
            end if;
            red_Blue_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(0)(even_High downto even_Low));
            red_Blue_Interp4_Ops(PEL_D)(pair_Index) <= unsigned(window_Matrix(1)(2)(even_High downto even_Low));

            -- The 2-interpolated red / blue pels go to the even pair member
            red_Blue_Interp2X_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)(even_High downto even_Low));
            red_Blue_Interp2X_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(2)(even_High downto even_Low));
            red_Blue_Interp2Y_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(1)(odd_High downto odd_Low));
            if(pair_Index = 0) then
              red_Blue_Interp2Y_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(0)(1)(Window_Pels_Type'HIGH downto (Window_Pels_Type'LENGTH - PLANE_DEPTH)));
            else
              red_Blue_Interp2Y_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(1)((even_High - PLANE_DEPTH) downto (even_Low - PLANE_DEPTH)));
            end if;
          else
            -- Origin pixel pair immersed as : Column 1 | Column 0
            --                                 ---------|---------
            --                                    G X G | X
            --                                    Y G Y | G
            --                                    G X G | X
            --
            -- The 4-interpolated green pel goes to the even pair member
            green_Interp4_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)(even_High downto even_Low));
            green_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(1)(odd_High downto odd_Low));
            if(pair_Index = 0) then
              -- Obtain from the highest-order pel in column zero
              green_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(0)(1)(Window_Pels_Type'HIGH downto (Window_Pels_Type'LENGTH - PLANE_DEPTH)));
            else
              -- Obtain from the next lower-order odd pel
              green_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(1)(1)((even_High - PLANE_DEPTH) downto (even_Low - PLANE_DEPTH)));
            end if;
            green_Interp4_Ops(PEL_D)(pair_Index) <= unsigned(window_Matrix(1)(2)(even_High downto even_Low));
            
            -- The 4-interpolated red / blue pel goes to the even pair member as well
            if(pair_Index = 0) then
              red_Blue_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(0)(0)(Window_Pels_Type'HIGH downto (Window_Pels_Type'LENGTH - PLANE_DEPTH)));
              red_Blue_Interp4_Ops(PEL_D)(pair_Index) <= unsigned(window_Matrix(0)(2)(Window_Pels_Type'HIGH downto (Window_Pels_Type'LENGTH - PLANE_DEPTH)));
            else
              red_Blue_Interp4_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(0)((even_High - PLANE_DEPTH) downto (even_Low - PLANE_DEPTH)));
              red_Blue_Interp4_Ops(PEL_D)(pair_Index) <= unsigned(window_Matrix(1)(2)((even_High - PLANE_DEPTH) downto (even_Low - PLANE_DEPTH)));
            end if;
            red_Blue_Interp4_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)(odd_High downto odd_Low));
            red_Blue_Interp4_Ops(PEL_C)(pair_Index) <= unsigned(window_Matrix(1)(2)(odd_High downto odd_Low));

            -- The 2-interpolated red / blue pels go to the odd pair member
            red_Blue_Interp2X_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(0)(odd_High downto odd_Low));
            red_Blue_Interp2X_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(2)(odd_High downto odd_Low));
            if(pair_Index = (NUM_PIXEL_PAIRS - 1)) then
              red_Blue_Interp2Y_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(2)(1)((PLANE_DEPTH - 1) downto 0));
            else
              red_Blue_Interp2Y_Ops(PEL_A)(pair_Index) <= unsigned(window_Matrix(1)(1)((odd_High + PLANE_DEPTH) downto (odd_Low + PLANE_DEPTH)));
            end if;
            red_Blue_Interp2Y_Ops(PEL_B)(pair_Index) <= unsigned(window_Matrix(1)(1)(even_High downto even_Low));
          end if; -- if(window green pixel first)
        end loop; -- for(each input pixel pair)
      end if; -- if(Window_Valid)

      -- Update the delay taps for the copied-through pel values
      copy_Pels_d <= (copy_Pels & copy_Pels_d(1 to (copy_Pels_d'HIGH - 1)));
      
    end if;
  end process;

  -- Infer the start and end of the window from other flags
  window_Start <= (Window_Top and Window_Replicate_Left);
  window_End   <= (Window_Bottom and Window_Replicate_Right);


  --
  -- Replication of interpolation primitives
  --
  -- Each pair of pixels requires the interpolation of four color plane
  -- values:
  --
  --   * A green pel interpolated from four pels in a star pattern
  --   * A blue or red pel interpolated from four pels in an X pattern
  --   * A blue or red pel interpolated from two horizontal neighbors
  --   * A blue or red pel interpolated from two vertial neighbors
  --
  -- A structural primitive is instantiated for each of the four above
  -- pel values, for each pixel pair.
  --
  interpolators : for pair_Index in 0 to (NUM_PIXEL_PAIRS - 1) generate

    -- Constants for slicing the pel operands
    constant operand_Low  : natural  := (pair_Index * PLANE_DEPTH);
    constant operand_High : positive := (operand_Low + PLANE_DEPTH - 1);

    -- Common signals for the interpolator primitives
    signal interp_Enable : std_logic;
    signal pair_Parity   : std_logic;
    signal dither_Pair   : std_logic;
    
  begin

    -- Enable as input pels are multiplexed to their appropriate operand(s)
    interp_Enable <= window_Valid_d(1);

    -- Dither in a checkerboard pattern, rounding up for all pixel pairs with
    -- both even column and row parity. Rounding decisions are made in pairs,
    -- as each decision affects both a green and a blue / red source pel location.
    --
    -- The absolute alignment of the dithering checkerboard is not critical
    -- nor specified; therefore, the red row is arbitrarily chosen for even-parity
    -- pixel pairs to receive a carry-in.
    pair_Parity <= '1' when ((pair_Index mod 2) = 1) else '0';
    dither_Pair <= (window_Red_Row_d(1) and not pair_Parity);

    --
    -- Green four-pel interpolator
    --
    green_4_Interp : entity work.Bayer_Four_Pel_Interp
    generic map
    (
      PEL_DEPTH => PLANE_DEPTH
    )
    port map
    (
      -- Pel clock and enable
      Pel_Clock => Pixel_Clock,
      Enable    => interp_Enable,
    
      -- Input operand pels and carry-in for dithering
      Pel_A    => green_Interp4_Ops(PEL_A)(pair_Index),
      Pel_B    => green_Interp4_Ops(PEL_B)(pair_Index),
      Pel_C    => green_Interp4_Ops(PEL_C)(pair_Index),
      Pel_D    => green_Interp4_Ops(PEL_D)(pair_Index),
      Carry_In => dither_Pair,
    
      -- Interpolated output pel
      Pel_Interp => green_Interp4(pair_Index)
    );

    
    --
    -- Red / blue four-pel interpolator
    --
    red_Blue_4_Interp : entity work.Bayer_Four_Pel_Interp
    generic map
    (
      PEL_DEPTH => PLANE_DEPTH
    )
    port map
    (
      -- Pel clock and enable
      Pel_Clock => Pixel_Clock,
      Enable    => interp_Enable,
    
      -- Input operand pels and carry-in for dithering
      Pel_A    => red_Blue_Interp4_Ops(PEL_A)(pair_Index),
      Pel_B    => red_Blue_Interp4_Ops(PEL_B)(pair_Index),
      Pel_C    => red_Blue_Interp4_Ops(PEL_C)(pair_Index),
      Pel_D    => red_Blue_Interp4_Ops(PEL_D)(pair_Index),
      Carry_In => dither_Pair,
    
      -- Interpolated output pel
      Pel_Interp => red_Blue_Interp4(pair_Index)
    );

    
    --
    -- Red / blue two-pel interpolator for 'X' pels
    --
    red_Blue_2X_Interp : entity work.Bayer_Two_Pel_Interp
    generic map
    (
      PEL_DEPTH => PLANE_DEPTH
    )
    port map
    (
      -- Pel clock and enable
      Pel_Clock => Pixel_Clock,
      Enable    => interp_Enable,
    
      -- Input operand pels and carry-in for dithering
      Pel_A    => red_Blue_Interp2X_Ops(PEL_A)(pair_Index),
      Pel_B    => red_Blue_Interp2X_Ops(PEL_B)(pair_Index),
      Carry_In => dither_Pair,
    
      -- Interpolated output pel
      Pel_Interp => red_Blue_Interp2X(pair_Index)
    );

    
    --
    -- Red / blue two-pel interpolator for 'Y' pels
    --
    red_Blue_2Y_Interp : entity work.Bayer_Two_Pel_Interp
    generic map
    (
      PEL_DEPTH => PLANE_DEPTH
    )
    port map
    (
      -- Pel clock and enable
      Pel_Clock => Pixel_Clock,
      Enable    => interp_Enable,
    
      -- Input operand pels and carry-in for dithering
      Pel_A    => red_Blue_Interp2Y_Ops(PEL_A)(pair_Index),
      Pel_B    => red_Blue_Interp2Y_Ops(PEL_B)(pair_Index),
      Carry_In => dither_Pair,
    
      -- Interpolated output pel
      Pel_Interp => red_Blue_Interp2Y(pair_Index)
    );

  end generate;

  
  --
  -- Output pel assignment
  --
  assign_Outputs : process(Pixel_Clock)
    variable even_Index : natural;
    variable odd_Index  : natural;
    variable even_High  : natural;
    variable even_Low   : natural;
    variable odd_High   : natural;
    variable odd_Low    : natural;
  begin
    if rising_edge(Pixel_Clock) then
  
      -- Drive out the final delay tap of each strobe to accompany the stream
      -- of interpolated pixels
      Output_Valid <= window_Valid_d(window_Valid_d'HIGH);
      Output_Start <= window_Start_d(window_Start_d'HIGH);
      Output_End   <= window_End_d(window_End_d'HIGH);

      -- Clock-enable the process upon valid interpolation / delay outputs
      if ?? window_Valid_d(window_Valid_d'HIGH) then
        -- Loop over each pair of pixels to be interpolated
        for pair_Index in 0 to (NUM_PIXEL_PAIRS - 1) loop
          -- Offset into the output vector for each pixel within the pair
          even_Index := (2 * pair_Index);
          odd_Index  := (even_Index + 1);
          even_Low   := (even_Index * PLANE_DEPTH);
          even_High  := (even_Low + PLANE_DEPTH - 1);
          odd_Low    := (odd_Index * PLANE_DEPTH);
          odd_High   := (odd_Low + PLANE_DEPTH - 1);

          -- Inspect the permutation of the input color plane parities and assign
          -- the data for each pair, expanding into RGB color space.
          if ?? window_Red_Row_d(window_Red_Row_d'HIGH) then
            -- Red row
            if ?? window_Green_First_d(window_Green_First_d'HIGH) then
              -- Origin pixel pair immersed as : Column 1 | Column 0
              --                                 ---------|---------
              --                                    B G B | G
              --                                    G R G | R
              --                                    B G B | G
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(odd_High downto odd_Low));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(red_Blue_Interp2Y(pair_Index));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(green_Interp4(pair_Index));
              Output_Green(even_High downto even_Low) <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(even_High downto even_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(red_Blue_Interp4(pair_Index));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(red_Blue_Interp2X(pair_Index));
            else
              -- Origin pixel pair immersed as : Column 1 | Column 0
              --                                 ---------|---------
              --                                    G B G | B
              --                                    R G R | G
              --                                    G B G | B
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(red_Blue_Interp2Y(pair_Index));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(even_High downto even_Low));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(odd_High downto odd_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(green_Interp4(pair_Index));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(red_Blue_Interp2X(pair_Index));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(red_Blue_Interp4(pair_Index));
            end if; -- if(green first)
          else
            -- Blue row
            if ?? window_Green_First_d(window_Green_First_d'HIGH) then
              -- Origin pixel pair immersed as : Column 1 | Column 0
              --                                 ---------|---------
              --                                    R G R | G
              --                                    G B G | B
              --                                    R G R | G
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(red_Blue_Interp4(pair_Index));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(red_Blue_Interp2X(pair_Index));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(green_Interp4(pair_Index));
              Output_Green(even_High downto even_Low) <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(even_High downto even_Low));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(odd_High downto odd_Low));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(red_Blue_Interp2Y(pair_Index));
            else
              -- Origin pixel pair immersed as : Column 1 | Column 0
              --                                 ---------|---------
              --                                    G R G | R
              --                                    B G B | G
              --                                    G R G | R
              Output_Red(odd_High downto odd_Low)     <= std_logic_vector(red_Blue_Interp2X(pair_Index));
              Output_Red(even_High downto even_Low)   <= std_logic_vector(red_Blue_Interp4(pair_Index));
              Output_Green(odd_High downto odd_Low)   <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(odd_High downto odd_Low));
              Output_Green(even_High downto even_Low) <= std_logic_vector(green_Interp4(pair_Index));
              Output_Blue(odd_High downto odd_Low)    <= std_logic_vector(red_Blue_Interp2Y(pair_Index));
              Output_Blue(even_High downto even_Low)  <= std_logic_vector(copy_Pels_d(copy_Pels_d'HIGH)(even_High downto even_Low));
            end if; -- if(green first)
          end if; -- if(red row)
        end loop; -- for(each output pixel pair)
      end if; -- if(interpolator outputs valid)
    end if;
  end process;

end architecture;
