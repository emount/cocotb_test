-- File Name   : Bayer_Demosaic_Balance.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Submodule responsible for white balance of the Bayer
--               demosaic core's output pels.
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


-- Entity declaration
entity Bayer_Demosaic_Balance is
  generic
  (
    -- PEL_DEPTH        - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK - Number of horizontal pixel values per clock
    PEL_DEPTH        : positive;
    PIXELS_PER_CLOCK : positive
  );
  port
  (
    -- Pixel reset and clock
    Pixel_Reset : in std_logic;
    Pixel_Clock : in std_logic;

    -- Interface to the register file
    White_Balance_Gains : in White_Balance_Gain_Array((NUM_RGB_PLANES - 1) downto 0);

    -- Input RGB stream
    Input_Valid : in std_logic;
    Input_Start : in std_logic;
    Input_End   : in std_logic;
    Input_Red   : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Input_Green : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Input_Blue  : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);

    -- Output RGB stream
    Output_Valid : out std_logic;
    Output_Start : out std_logic;
    Output_End   : out std_logic;
    Output_Red   : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Output_Green : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Output_Blue  : out std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0)
  );
end entity;
  

-- Architecture implementation
architecture rtl of Bayer_Demosaic_Balance is

  -- Wrapper signals for the white balance operation
  signal input_Valid_d : std_logic;
  signal input_Start_d : std_logic;
  signal input_End_d   : std_logic;
  
  subtype Signed_Gain_Type is signed(WHITE_BALANCE_BITS downto 0);
  type Signed_Gain_Array is array (natural range <>) of Signed_Gain_Type;
  signal signed_Gains : Signed_Gain_Array(White_Balance_Gains'RANGE);
  
  subtype Signed_Pel_Type is signed(PEL_DEPTH downto 0);
  type Signed_Pel_Array is array (natural range <>) of Signed_Pel_Type;
  signal input_Red_Pels   : Signed_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal input_Green_Pels : Signed_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal input_Blue_Pels  : Signed_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);

  constant SCALED_PRODUCT_WIDTH : positive := (Signed_Gain_Type'LENGTH + Signed_Pel_Type'LENGTH);
  subtype Scaled_Product_Type is signed((SCALED_PRODUCT_WIDTH - 1) downto 0);
  type Scaled_Product_Array is array (natural range <>) of Scaled_Product_Type;
  signal red_Products   : Scaled_Product_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal green_Products : Scaled_Product_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal blue_Products  : Scaled_Product_Array((PIXELS_PER_CLOCK - 1) downto 0);

  subtype Unsigned_Pel_Type is unsigned((PEL_DEPTH - 1) downto 0);
  type Unsigned_Pel_Array is array (natural range <>) of Unsigned_Pel_Type;
  signal red_Balanced   : Unsigned_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal green_Balanced : Unsigned_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal blue_Balanced  : Unsigned_Pel_Array((PIXELS_PER_CLOCK - 1) downto 0);
  
  -- Constants and signals for saturation detection; the two extended sign bits
  -- effectively behave as guard bits for the multiplication, while the gain-
  -- multiplied pel values must never grow into the bit represented by the gain
  -- mantissa.
  constant OVERFLOW_BITS   : positive := 3;
  constant NO_OVERFLOW     : std_logic_vector((OVERFLOW_BITS - 1) downto 0) := (others => '0');
  constant SATURATED_PIXEL : std_logic_vector(Unsigned_Pel_Type'RANGE)      := (others => '1');
  subtype Overflow_Field_Type is std_logic_vector((OVERFLOW_BITS - 1) downto 0);
  type Overflow_Field_Array is array (natural range <>) of Overflow_Field_Type;
  signal red_Overflow_Fields   : Overflow_Field_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal green_Overflow_Fields : Overflow_Field_Array((PIXELS_PER_CLOCK - 1) downto 0);
  signal blue_Overflow_Fields  : Overflow_Field_Array((PIXELS_PER_CLOCK - 1) downto 0);
    
begin

  --
  -- Sign-extend the white balance gain for each plane
  --
  extend_Gains : process(all)
  begin
    for plane_Index in signed_Gains'RANGE loop
      signed_Gains(plane_Index) <= signed('0' & White_Balance_Gains(plane_Index));
    end loop;
  end process;
  

  --
  -- Split out the input vectors into sign-extended multiplicands
  --
  split_Input : process(all)
  begin
    for pixel_Index in input_Red_Pels'RANGE loop
      input_Red_Pels(pixel_Index) <=
        signed('0' & Input_Red((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)));
      input_Green_Pels(pixel_Index) <=
        signed('0' & Input_Green((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)));
      input_Blue_Pels(pixel_Index) <=
        signed('0' & Input_Blue((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)));
    end loop;
  end process;
  

  --
  -- Process implementing the white balance operation
  --
  white_Balance : process(Pixel_Reset, Pixel_Clock)
  begin
    if ?? Pixel_Reset then
      Output_Valid <= '0';
      Output_Start <= '0';
      Output_End   <= '0';
    elsif rising_edge(Pixel_Clock) then

      -- Forward the strobes through a delay stage for the products
      input_Valid_d <= Input_Valid;
      input_Start_d <= Input_Start;
      input_End_d   <= Input_End;
      
      -- Compute the scaled products for each color plane
      for pixel_Index in input_Red_Pels'RANGE loop

        red_Products(pixel_Index) <=
          resize((signed_Gains(RGB_PLANE_RED) * input_Red_Pels(pixel_Index)),
                 Scaled_Product_Type'LENGTH);

        green_Products(pixel_Index) <=
          resize((signed_Gains(RGB_PLANE_GREEN) * input_Green_Pels(pixel_Index)),
                 Scaled_Product_Type'LENGTH);

        blue_Products(pixel_Index) <=
          resize((signed_Gains(RGB_PLANE_BLUE) * input_Blue_Pels(pixel_Index)),
                 Scaled_Product_Type'LENGTH);

      end loop;

      -- Assign the output strobes
      Output_Valid <= input_Valid_d;
      Output_Start <= input_Start_d;
      Output_End   <= input_End_d;

      -- Multiplex between the weighted values and a saturated pixel value
      -- based upon whether overflow has taken place at each pixel
      for pixel_Index in red_Balanced'RANGE loop
        if(red_Overflow_Fields(pixel_Index) = NO_OVERFLOW) then
          Output_Red((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            std_logic_vector(red_Balanced(pixel_Index));
        else
          Output_Red((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            SATURATED_PIXEL;
        end if;

        if(green_Overflow_Fields(pixel_Index) = NO_OVERFLOW) then
          Output_Green((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            std_logic_vector(green_Balanced(pixel_Index));
        else
          Output_Green((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            SATURATED_PIXEL;
        end if;
        
        if(blue_Overflow_Fields(pixel_Index) = NO_OVERFLOW) then
          Output_Blue((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            std_logic_vector(blue_Balanced(pixel_Index));
        else
          Output_Blue((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH)) <=
            SATURATED_PIXEL;
        end if;
      end loop;
      
    end if;
  end process;


  --
  -- Split out the raw products into overflow detection fields and balanced pel
  -- values on each color plane for multiplexing to the outputs
  --
  split_Overflow : process(all)
  begin
    for pixel_Index in red_Products'RANGE loop
      red_Overflow_Fields(pixel_Index) <=
        std_logic_vector(red_Products(pixel_Index)(Scaled_Product_Type'HIGH downto
                                                   (Scaled_Product_Type'LENGTH - OVERFLOW_BITS)));
      red_Balanced(pixel_Index) <=
        unsigned(red_Products(pixel_Index)((PEL_DEPTH + WHITE_BALANCE_FRACT_BITS - 1) downto
                                           WHITE_BALANCE_FRACT_BITS));
      green_Overflow_Fields(pixel_Index) <=
        std_logic_vector(green_Products(pixel_Index)(Scaled_Product_Type'HIGH downto
                                                     (Scaled_Product_Type'LENGTH - OVERFLOW_BITS)));
      green_Balanced(pixel_Index) <=
        unsigned(green_Products(pixel_Index)((PEL_DEPTH + WHITE_BALANCE_FRACT_BITS - 1) downto
                                             WHITE_BALANCE_FRACT_BITS));
      blue_Overflow_Fields(pixel_Index) <=
        std_logic_vector(blue_Products(pixel_Index)(Scaled_Product_Type'HIGH downto
                                                    (Scaled_Product_Type'LENGTH - OVERFLOW_BITS)));
      blue_Balanced(pixel_Index) <=
        unsigned(blue_Products(pixel_Index)((PEL_DEPTH + WHITE_BALANCE_FRACT_BITS - 1) downto
                                            WHITE_BALANCE_FRACT_BITS));
    end loop;
  end process;

end architecture;
