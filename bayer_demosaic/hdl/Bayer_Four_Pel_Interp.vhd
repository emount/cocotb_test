-- File Name   : Bayer_Four_Pel_Interp.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Structural submodule for dithered interpolation of four
--               pixel elements (pels).
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
entity Bayer_Four_Pel_Interp is
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
    Pel_C    : in unsigned((PEL_DEPTH - 1) downto 0);
    Pel_D    : in unsigned((PEL_DEPTH - 1) downto 0);
    Carry_In : in std_logic;

    -- Interpolated output pel
    Pel_Interp : out unsigned((PEL_DEPTH - 1) downto 0)
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Four_Pel_Interp is

  -- Pipeline constants and signals
  constant NO_CARRY    : std_logic := '0';
  signal pels_Sum_AB   : unsigned(PEL_DEPTH downto 0);
  signal pels_Sum_CD   : unsigned(PEL_DEPTH downto 0);
  signal enable_d      : std_logic;
  signal carry_In_d    : std_logic;
  signal pels_Sum_Full : unsigned((PEL_DEPTH + 1) downto 0);
  
begin

  --
  -- Cascaded instantiations of a two-pel adder with carry
  --
  -- The input operands are processed in parallel as pairs, whose
  -- respective sums feed the second, dithered output stage.
  --

  -- (PEL_A + PEL_B)
  pel_Adder_AB : entity work.Bayer_Two_Pel_Adder
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
    Carry_In => NO_CARRY,

    -- Output sum, with bit growth
    Pels_Sum => pels_Sum_AB
  );


  -- (PEL_C + PEL_D)
  pel_Adder_CD : entity work.Bayer_Two_Pel_Adder
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
    Pel_A    => Pel_C,
    Pel_B    => Pel_D,
    Carry_In => NO_CARRY,

    -- Output sum, with bit growth
    Pels_Sum => pels_Sum_CD
  );


  --
  -- Delay stage for both the input clock enable and the carry-in
  -- bit which accompanies it
  --
  enable_d   <= Enable when rising_edge(Pel_Clock);
  carry_In_d <= Carry_In when rising_edge(Pel_Clock);


  -- [(PEL_A + PEL_B) + (PEL_C + PEL_D) + Carry_In]
  --
  -- The second stage respects the systolically-delayed clock enable
  pel_Adder_Full : entity work.Bayer_Two_Pel_Adder
  generic map
  (
    PEL_DEPTH => (PEL_DEPTH + 1)
  )
  port map
  (
    -- Pel clock and enable
    Pel_Clock => Pel_Clock,
    Enable    => enable_d,

    -- Input operand pels and carry-in for dithering
    Pel_A    => pels_Sum_AB,
    Pel_B    => pels_Sum_CD,
    Carry_In => carry_In_d,

    -- Output sum, with bit growth
    Pels_Sum => pels_Sum_Full
  );


  --
  -- Assign to the output vector, truncating the two bits of fraction
  --
  Pel_Interp <= pels_Sum_Full(pels_Sum_Full'HIGH downto 2);

end architecture;
