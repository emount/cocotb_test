-- File Name   : Bayer_Demosaic_Package.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Package containing constants, types, and functions for the
--               Bayer_Demosaic IP core.
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

-- Module packages
library work;
use work.Bayer_Demosaic_Kernels.all;
use work.CL_Video_Defs.all;


-- Package declaration
package Bayer_Demosaic_Package is

  --
  -- Revision number of the IP core
  --
  
  -- These are encoded in the device's capabilities register, enabling driver
  -- software to probe and compare against its support range of revisions.
  constant REVISION_FIELD_BITS           : positive := 8;
  constant BAYER_DEMOSAIC_REVISION_MAJOR : natural  := 1;
  constant BAYER_DEMOSAIC_REVISION_MINOR : natural  := 0;

  
  --
  -- Structural constant definitions
  --

  -- Latency of the nearest-neighbor interpolation kernel, which is always
  -- implemented (if only for the boundary pixels).
  constant BAYER_NEAREST_LATENCY : positive := 1;

  -- Latency of the bilinear interpolation kernel
  constant BAYER_BILINEAR_LATENCY : positive := 4;

  -- Function returning the latency associated with the passed interpolation
  -- kernel. This is only ever evaluated at elaboration (compile) time; that
  -- is to say, statically.
  --
  -- \param kernel - Kernel function to implement for intra-frame pixels
  --
  -- \return The systolic latency of the selected kernel
  function Bayer_Kernel_Latency(kernel : in Bayer_Kernel_Type) return positive;

  
  --
  -- Common types used within submodules
  --
  
  -- Enumerated types for identifying Bayer pattern parity options
  type Row_Mode_Type is (RED_FIRST, BLUE_FIRST);
  type Column_Mode_Type is (GREEN_FIRST, GREEN_SECOND);

  -- Conversion functions between enumerations and std_logic type
  function To_Std_Logic(row_Mode : in Row_Mode_Type) return std_logic;
  function To_Std_Logic(column_Mode : in Column_Mode_Type) return std_logic;
  function To_Row_Mode(bit_Value : in std_logic) return Row_Mode_Type;
  function To_Column_Mode(bit_Value : in std_logic) return Column_Mode_Type;

  -- Type definitions for white balancing operations
  constant WHITE_BALANCE_MANT_BITS  : positive := 1;
  constant WHITE_BALANCE_FRACT_BITS : positive := 15;
  constant WHITE_BALANCE_BITS       : positive := (WHITE_BALANCE_MANT_BITS +
                                                   WHITE_BALANCE_FRACT_BITS);
  subtype White_Balance_Gain_Type is unsigned((WHITE_BALANCE_BITS - 1) downto 0);
  type White_Balance_Gain_Array is array (natural range <>) of White_Balance_Gain_Type;
  constant WHITE_BALANCE_UNITY_GAIN : White_Balance_Gain_Type := To_Unsigned((2 ** WHITE_BALANCE_FRACT_BITS),
                                                                             WHITE_BALANCE_BITS);

  -- Utility function for converting a white balance value
  --
  -- With only a single mantissa bit, white balance values are intended to
  -- be within the inclusive range [0.0, 1.0]. Values exceeding unity are
  -- not intended for use, despite being representible. There is no clamping
  -- implemented for post-gain pel values, creating the possibility of
  -- numerical overflow and wrapping.
  --
  -- \param gain - Real-valued gain to transform into a native value
  --
  -- \return The native representation for the gain value
  function To_White_Balance_Gain(gain : in real) return White_Balance_Gain_Type;

  
  --
  -- Host interrupt declarations
  --

  constant NUM_IRQS    : positive := 1;
  constant IRQ_CAPTURE : natural  := 0;

  
  --
  -- Host register address and field types and constants
  --
    
  constant BAYER_ADDRESS_WIDTH : positive := 8;
  subtype Bayer_Host_Address is std_logic_vector((BAYER_ADDRESS_WIDTH - 1) downto 0);
  subtype Bayer_Register_Value is std_logic_vector(31 downto 0);

  -- Control register
  constant BAYER_CTRL_REG_ADDRESS      : Bayer_Host_Address := x"00";
    constant BAYER_ENABLE_BIT   : natural := Bayer_Register_Value'HIGH;
      constant BAYER_DISABLE : Bayer_Register_Value := x"00000000";
      constant BAYER_ENABLE  : Bayer_Register_Value := x"80000000";
    constant BAYER_COL_MODE_BIT : natural := 2;
      constant BAYER_COL_GREEN_FIRST  : Bayer_Register_Value := x"00000000";
      constant BAYER_COL_GREEN_SECOND : Bayer_Register_Value := x"00000004";
    constant BAYER_ROW_MODE_BIT : natural := 1;
      constant BAYER_ROW_RED_FIRST  : Bayer_Register_Value := x"00000000";
      constant BAYER_ROW_BLUE_FIRST : Bayer_Register_Value := x"00000002";
    constant CAPTURE_CTRL_BIT   : natural := 0;
      constant CAPTURE_HEADER : Bayer_Register_Value := x"00000001";

  -- Interrupt flags and mask registers
  constant BAYER_IRQ_FLAGS_REG_ADDRESS : Bayer_Host_Address := x"04";
  constant BAYER_IRQ_MASK_REG_ADDRESS  : Bayer_Host_Address := x"05";

  -- Video header capture registers
  constant BAYER_RES_CAPTURE_ADDRESS   : Bayer_Host_Address := x"08";
  constant BAYER_ROI_CAPTURE_ADDRESS   : Bayer_Host_Address := x"09";
  constant BAYER_INDEX_CAPTURE_ADDRESS : Bayer_Host_Address := x"0A";
  constant BAYER_TS_CAPTURE_ADDRESS    : Bayer_Host_Address := x"0B";

  -- White balance gain registers
  constant BAYER_WHITE_BALANCE_RED     : Bayer_Host_Address := x"0C";
  constant BAYER_WHITE_BALANCE_GREEN   : Bayer_Host_Address := x"0D";
  constant BAYER_WHITE_BALANCE_BLUE    : Bayer_Host_Address := x"0E";

  -- Core ID register
  constant BAYER_ID_REG_ADDRESS        : Bayer_Host_Address := x"80";
    -- The ID word is sha256("bayer_demosaic")[31:0]
    constant BAYER_ID_WORD : std_logic_vector(31 downto 0) := x"3AEFEE02";

  -- Core revision register
  constant BAYER_REVISION_REG_ADDRESS  : Bayer_Host_Address := x"81";

  -- Core capabilities register
  constant BAYER_CAPS_REG_ADDRESS      : Bayer_Host_Address := x"82";
    constant BP_SUPPORT_CAPS_BIT   : natural  := 31;
    constant PEL_DEPTH_CAPS_BITS   : positive :=  4;
    constant PIX_PER_CLK_CAPS_BITS : positive :=  4;
  
end package;


-- Package body definition
package body Bayer_Demosaic_Package is

  --
  -- Function bodies
  --
  
  function Bayer_Kernel_Latency(kernel : in Bayer_Kernel_Type) return positive is
    variable latency : positive;
  begin
    
    -- This case statement encodes the latency, in Pixel_Clock cycles, of each
    -- of the interpolation kernels supported by the kernel selector architecture.
    -- Specifically, this is the latency from the cycle in which it is primed to the
    -- first output pixel of inerpolated data.
    --
    -- As such, this must be kept consistent with the respective implementations.
    case kernel is
      when BAYER_INTERP_BILINEAR =>
        -- Systolic latency of the bi-linear interpolation kernel
        latency := BAYER_BILINEAR_LATENCY;
        
      when others =>
        -- Latency of the nearest-neighbor kernel
        latency := BAYER_NEAREST_LATENCY;
        
    end case;
        
    return latency;
  end function;

  
  function To_White_Balance_Gain(gain : in real) return White_Balance_Gain_Type is
  begin
    return To_Unsigned(integer(gain * (2.0 ** WHITE_BALANCE_FRACT_BITS)), WHITE_BALANCE_BITS);
  end function;

  
  function To_Std_Logic(row_Mode : in Row_Mode_Type) return std_logic is
    variable bit_Value : std_logic;
  begin
    bit_Value := '0';
    if(row_Mode = BLUE_FIRST) then
      bit_Value := '1';
    end if;

    return bit_Value;
  end function;

  
  function To_Std_Logic(column_Mode : in Column_Mode_Type) return std_logic is
    variable bit_Value : std_logic;
  begin
    bit_Value := '0';
    if(column_Mode = GREEN_SECOND) then
      bit_Value := '1';
    end if;

    return bit_Value;
  end function;

  
  function To_Row_Mode(bit_Value : in std_logic) return Row_Mode_Type is
    variable row_Mode : Row_Mode_Type;
  begin
    row_Mode := RED_FIRST;
    if(bit_Value = '1') then
      row_Mode := BLUE_FIRST;
    end if;

    return row_Mode;
  end function;

  
  function To_Column_Mode(bit_Value : in std_logic) return Column_Mode_Type is
    variable column_Mode : Column_Mode_Type;
  begin
    column_Mode := GREEN_FIRST;
    if(bit_Value = '1') then
      column_Mode := GREEN_SECOND;
    end if;

    return column_Mode;
  end function;
  
end package body;
