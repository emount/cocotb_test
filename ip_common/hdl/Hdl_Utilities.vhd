-- File Name   : Hdl_Utilities.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Basic utilities package
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;


-- Package declaration
package Hdl_Utilities is

  --
  -- Some useful basic unconstrained array types
  --
  type Integer_Array is array (natural range <>) of integer;
  type Positive_Array is array (natural range <>) of positive;
  type Natural_Array is array (natural range <>) of natural;
  type Real_Array is array (natural range <>) of real;
  

  -- Function to return the minimum number of bits necessary to represent a value
  --
  -- @param int_Value - Value to be represented
  --
  -- @return The number of bits necessary to represent the value
  function Min_Repr_Bits(int_Value : in natural) return positive;

end package;


-- Package body definition
package body Hdl_Utilities is

  --
  -- Function bodies
  --

  function Min_Repr_Bits(int_Value : in natural) return positive is
    variable min_Bits : positive := 1;
    variable shifter  : natural  := int_Value;
  begin
    shift_Loop : loop
      shifter := (shifter / 2);
      if(shifter = 0) then exit shift_Loop; end if;
      min_Bits := (min_Bits + 1);
    end loop;

    return min_Bits;
  end function;
  
end package body;
