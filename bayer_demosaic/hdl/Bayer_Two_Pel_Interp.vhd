-- File Name   : Bayer_Two_Pel_Interp.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Structural submodule for dithered interpolation of two
--               pixel elements (pels). The systolic delay of this module
--               is tailored to match that of the four-pel interpolator.
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


-- Entity declaration
entity Bayer_Two_Pel_Interp is
  generic
  (
    -- PEL_DEPTH - Depth of each pixel element, in bits
    PEL_DEPTH : positive
  );
  port
  (
    -- Pel clock and enable
    Pel_Clock : in std_logic;
    Enable    : in std_logic;

    -- Input operand pels and carry-in for dithering
    Pel_A    : in unsigned((PEL_DEPTH - 1) downto 0);
    Pel_B    : in unsigned((PEL_DEPTH - 1) downto 0);
    Carry_In : in std_logic;

    -- Interpolated output pel
    Pel_Interp : out unsigned((PEL_DEPTH - 1) downto 0)
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Two_Pel_Interp is

  -- Pipeline signals
  signal pels_Sum : unsigned(PEL_DEPTH downto 0);
  signal enable_d : std_logic;
  
begin

  --
  -- Structural instantiation of a two-pel adder with carry
  --
  pel_Adder : entity work.Bayer_Two_Pel_Adder
  generic map
  (
    PEL_DEPTH => PEL_DEPTH
  )
  port map
  (
    -- Pel clock and enable
    Pel_Clock => Pel_Clock,
    Enable    => Enable,

    -- Input operand pels and carry-in for dithering
    Pel_A    => Pel_A,
    Pel_B    => Pel_B,
    Carry_In => Carry_In,

    -- Output sum, with bit growth
    Pels_Sum => pels_Sum
  );

  
  --
  -- Extra delay stage for alignment with the four-pel interpolator
  --
  -- Truncate the single bit of fraction in the output assignment
  --
  enable_d   <= Enable when rising_edge(Pel_Clock);
  Pel_Interp <= pels_Sum(pels_Sum'HIGH downto 1) when rising_edge(Pel_Clock) and (enable_d = '1');

end architecture;
