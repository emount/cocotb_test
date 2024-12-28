-- File Name   : Bayer_Demosaic_Kernels.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Package defining the interpolation kernels available for use with
--               the bayer_demosaic core, as well as core interpolation functions
--               for use across one or more kernel concretions.
--
--               An entity declaration is made for all interpolation kernels to
--               adhere to, along with an architecture capable of multiplexing
--               between different kernels internally.
--
--               A convenience function is also supplied to facilitate integration
--               of the core into system-level design tools.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE standard packages
library ieee;
use ieee.numeric_std.all;


-- Package declaration
package Bayer_Demosaic_Kernels is

  --
  -- Enumerated type identifying the available convolution kernels
  --
  
  -- NOTE - Nearest-neighbor interpolation is implemented on the outer two
  --        boundary pixels of each frame, independent of the core interpolation
  --        kernel selected for any particular instance.
  --
  type Bayer_Kernel_Type is (BAYER_INTERP_NEAREST, BAYER_INTERP_BILINEAR);

  
  -- Function to convert from an ordinal number to its corresponding interpolation
  -- kernel type.
  --
  -- \param kernel_Index - Index of the desired interpolation kernel
  --
  -- \return The corresponding Bayer_Kernel_Tye enumerated value
  function To_Bayer_Kernel_Type(kernel_Index : in natural) return Bayer_Kernel_Type;

  -- Function returning the number of lines of window depth required by the
  -- passed interpolation kernel. This is only to be evaluated at elaboration.
  --
  -- \param kernel - Kernel function to implement for intra-frame pixels
  --
  -- \return The number of lines of window required by the selected kernel
  function Bayer_Kernel_Window_Size(kernel : in Bayer_Kernel_Type) return positive;


  --
  -- Generalized interpolation functions
  --

  -- \brief Function evenly interpolating two pel values of any bit width
  --
  -- In this context, "evenly" refers to the relationship of the two input
  -- pels' coordinates: they are defined as equidistant from the output pel.
  --
  -- \param pel_Even - Even-parity pel to be interpolated between
  -- \param pel_Odd  - Odd-parity pel to be interpolated between
  -- \param round    - Flag indicating whether to perform rounding
  --
  -- \return The interpolated pel, in the same precision as operands
  function Bayer_Interp_Two_Even(pel_Even : in unsigned;
                                 pel_Odd  : in unsigned;
                                 round    : in boolean) return unsigned;
  
end package;


-- Package body definition
package body Bayer_Demosaic_Kernels is

  --
  -- Function bodies
  --
  
  function To_Bayer_Kernel_Type(kernel_Index : in natural) return Bayer_Kernel_Type is
    variable kernel : Bayer_Kernel_Type;
  begin

    case kernel_Index is
      when 1 =>
        kernel := BAYER_INTERP_BILINEAR;

      when others =>
        kernel := BAYER_INTERP_NEAREST;
    end case;

    return kernel;
  end function;

  
  function Bayer_Kernel_Window_Size(kernel : in Bayer_Kernel_Type) return positive is
    variable window_Size : positive;
  begin
    
    case kernel is
      when BAYER_INTERP_BILINEAR =>
        -- Window size of the bilinear kernel
        window_Size := 3;
        
      when others =>
        -- Window size of the nearest-neighbor kernel
        window_Size := 2;

    end case;
        
    return window_Size;
  end function;

  function Bayer_Interp_Two_Even(pel_Even : in unsigned;
                                 pel_Odd  : in unsigned;
                                 round    : in boolean) return unsigned is
    constant pad : unsigned(0 downto 0) := To_Unsigned(0, 1);
    variable sum : unsigned(pel_Even'LENGTH downto 0);
  begin
    sum := ((pad & pel_Even) + (pad & pel_Odd));
    if round then sum := (sum + 1); end if;

    return sum(sum'HIGH downto 1);
  end function;

end package body;


-- IEEE standard libraries
library ieee;
use ieee.std_logic_1164.all;

-- Inclusion of the above package
use work.Bayer_Demosaic_Kernels.all;


--
-- Entity declaration for a generic kernel
--
entity Bayer_Kernel is
  generic
  (
    -- PIXELS_PER_CLOCK - Number of horizontal pixel values per clock
    -- PLANE_DEPTH      - Depth of source and output pixel planes, in bits
    -- WINDOW_SIZE      - Size of the window (N x N) presented to the kernel
    -- EXTRA_LATENCY    - "Extra" latency to cause the kernel to incur
    PIXELS_PER_CLOCK : positive;
    PLANE_DEPTH      : positive;
    WINDOW_SIZE      : positive;
    EXTRA_LATENCY    : natural := 0
  );
  port
  (
    -- Pixel reset and clock
    Pixel_Reset : in std_logic;
    Pixel_Clock : in std_logic;

    -- Pixel window for interpolation
    --
    -- The window presented is sized for the top-level requirements
    -- of the pipeline as a whole. Each architecture internally splits
    -- out only the pixels it requires for operation, properly origin-
    -- located.
    Window_Valid           : in std_logic;
    Window_Top             : in std_logic;
    Window_Bottom          : in std_logic;
    Window_Replicate_Row   : in std_logic;
    Window_Replicate_Left  : in std_logic;
    Window_Replicate_Right : in std_logic;
    Window_Red_Row         : in std_logic;
    Window_Green_First     : in std_logic;
    Window_Pixels          : in std_logic_vector((((WINDOW_SIZE ** 2) * PIXELS_PER_CLOCK * PLANE_DEPTH) - 1) downto 0);

    -- Interpolated output pixel stream
    Output_Valid  : out std_logic;
    Output_Start  : out std_logic;
    Output_End    : out std_logic;
    Output_Red    : out std_logic_vector(((PIXELS_PER_CLOCK * PLANE_DEPTH) - 1) downto 0);
    Output_Green  : out std_logic_vector(((PIXELS_PER_CLOCK * PLANE_DEPTH) - 1) downto 0);
    Output_Blue   : out std_logic_vector(((PIXELS_PER_CLOCK * PLANE_DEPTH) - 1) downto 0)
  );
end entity;
