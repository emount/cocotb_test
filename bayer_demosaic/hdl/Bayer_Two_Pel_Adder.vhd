-- File Name   : Bayer_Two_Pel_Adder.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Structural adder submodule for pixel elements (pels).
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
entity Bayer_Two_Pel_Adder is
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

    -- Output sum, with bit growth
    Pels_Sum : out unsigned(PEL_DEPTH downto 0)
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Two_Pel_Adder is

  -- Wrapper signals
  signal pel_A_Extended : unsigned(PEL_DEPTH downto 0);
  signal pel_B_Extended : unsigned(PEL_DEPTH downto 0);
  
begin

  -- Extend the input operands
  pel_A_Extended <= ('0' & pel_A);
  pel_B_Extended <= ('0' & pel_B);

  --
  -- Process implementing the adder, using a style conducive to mapping
  -- into dedicated ripple-carry adder logic
  --
  Pels_Sum <= (pel_A_Extended + pel_B_Extended + ("" & Carry_In)) when rising_edge(Pel_Clock) and (Enable = '1');

end architecture;
