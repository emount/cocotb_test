-- File Name   : Bayer_Arch_Selector_Kernel.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Structural wrapper capable of selecting a specific Bayer
--               interpolation kernel architecture based upon a passed
--               generic value.
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


--
-- Entity declaration for the architecture selector kernel
--
entity Bayer_Arch_Selector_Kernel is
  generic
  (
    -- INTERP_KERNEL    - Specific interpolation kernel to select
    -- PIXELS_PER_CLOCK - Number of horizontal pixel values per clock
    -- PLANE_DEPTH      - Depth of source and output pixel planes, in bits
    -- WINDOW_SIZE      - Size of the window (N x N) presented to the kernel
    INTERP_KERNEL    : Bayer_Kernel_Type;
    PIXELS_PER_CLOCK : positive;
    PLANE_DEPTH      : positive;
    WINDOW_SIZE      : positive
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


-- Architecture implementation
architecture structural of Bayer_Arch_Selector_Kernel is      
begin

  --
  -- Instantiate the appropriate Bayer kernel architecture
  --
  select_Kernel : if(INTERP_KERNEL = BAYER_INTERP_BILINEAR) generate

    --
    -- Instantiate the bilinear interpolation kernel for use
    --
    kernel : entity work.Bayer_Kernel(Bilinear)
    generic map
    (
      PIXELS_PER_CLOCK => PIXELS_PER_CLOCK,
      PLANE_DEPTH      => PLANE_DEPTH,
      WINDOW_SIZE      => WINDOW_SIZE
    )
    port map
    (
      -- Pixel reset and clock
      Pixel_Reset => Pixel_Reset,
      Pixel_Clock => Pixel_Clock,
    
      -- Pixel window for interpolation
      Window_Valid           => Window_Valid,
      Window_Top             => Window_Top,
      Window_Bottom          => Window_Bottom,
      Window_Replicate_Row   => Window_Replicate_Row,
      Window_Replicate_Left  => Window_Replicate_Left,
      Window_Replicate_Right => Window_Replicate_Right,
      Window_Red_Row         => Window_Red_Row,
      Window_Green_First     => Window_Green_First,
      Window_Pixels          => Window_Pixels,
    
      -- Interpolated output pixel stream
      Output_Valid  => Output_Valid,
      Output_Start  => Output_Start,
      Output_End    => Output_End,
      Output_Red    => Output_Red,
      Output_Green  => Output_Green,
      Output_Blue   => Output_Blue
    );
  
  else generate

    -- Default to nearest-neighbor
    --
    -- This case merely stubs off the outputs; the same NN kernel is used
    -- for core pixels as generates boundary pixel values.
    Output_Valid <= '0';
    Output_Start <= '0';
    Output_End   <= '0';
    Output_Red   <= (others => '0');
    Output_Green <= (others => '0');
    Output_Blue  <= (others => '0');
      
  end generate;
  
end architecture;
