-- File Name   : Bayer_Demosaic_Elastic_Fifo.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Module implementing a synchronous FIFO for providing 'elastic'
--               storage at the output stage of the Bayer demosaic core.
--
--               This module passes the FPGA family through to the wrapped
--               primitive used for simulation and synthesis.
--
--     o  0
--     | /       Copyright (c) 2018
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;

-- Altera Megafunction library
library altera_mf;
use altera_mf.all;

-- Utility package
library work;
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Elastic_Fifo is
  generic
  (
    -- FPGA_FAMILY - String indicating the FPGA family to compile for
    -- DATA_WIDTH  - Width of FIFO data words, in bits
    -- FIFO_DEPTH  - Depth of the FIFO, in words
    FPGA_FAMILY : string;
    DATA_WIDTH  : positive;
    FIFO_DEPTH  : positive
  );
  port
  (
    -- Common clock and synchronous reset
    Reset : in std_logic;
    Clock : in std_logic;
    
    -- Write interface
    Write_Enable : in  std_logic;
    Write_Start  : in  std_logic;
    Write_End    : in  std_logic;
    Write_Data   : in  std_logic_vector((DATA_WIDTH - 1) downto 0);
    Write_Full   : out std_logic;

    -- Read interface
    Read_Enable : in  std_logic;
    Read_Start  : out std_logic;
    Read_End    : out std_logic;
    Read_Data   : out std_logic_vector((DATA_WIDTH - 1) downto 0);
    Read_Empty  : out std_logic
  );
end entity;


-- Architecture definition
architecture Altera_Megafunction of Bayer_Demosaic_Elastic_Fifo is

  --
  -- Constants and wrapper signals for the FIFO
  --
  constant FIFO_USEDW_BITS : positive := Min_Repr_Bits(FIFO_DEPTH - 1);
  constant FIFO_DATA_WIDTH : positive := (DATA_WIDTH + 2);
  signal fifo_Write_Data   : std_logic_vector((FIFO_DATA_WIDTH - 1) downto 0);
  signal fifo_Read_Data    : std_logic_vector((FIFO_DATA_WIDTH - 1) downto 0);


  --
  -- Component declaration for the wrapped FIFO primitive
  --
  component scfifo
  generic
  (
    add_ram_output_register : string;
    intended_device_family  : string;
    lpm_numwords            : natural;
    lpm_showahead           : string;
    lpm_type                : string;
    lpm_width               : natural;
    lpm_widthu              : natural;
    overflow_checking       : string;
    underflow_checking      : string;
    use_eab                 : string
  );
  port
  (
    clock : in std_logic;
    usedw : out std_logic_vector((lpm_widthu - 1) downto 0);
    empty : out std_logic;
    full  : out std_logic;
    q     : out std_logic_vector((lpm_width - 1) downto 0);
    wrreq : in std_logic; 
    aclr  : in std_logic;
    data  : in std_logic_vector((lpm_width - 1) downto 0);
    rdreq : in std_logic
  );
  end component;
  
begin


  --
  -- Assign wrapper signals in and out of the module
  --
  fifo_Write_Data <= (Write_Start & Write_End & Write_Data);
  Read_Start      <= fifo_Read_Data(fifo_Read_Data'HIGH);
  Read_End        <= fifo_Read_Data(fifo_Read_Data'HIGH - 1);
  Read_Data       <= fifo_Read_Data((fifo_Read_Data'HIGH - 2) downto 0);


  --
  -- Instantiate the wrapped FIFO primitive
  --
  wrapped_Fifo : scfifo
  generic map
  (
    add_ram_output_register => "OFF",
    intended_device_family  => FPGA_FAMILY,
    lpm_numwords            => FIFO_DEPTH,
    lpm_showahead           => "OFF",
    lpm_type                => "scfifo",
    lpm_width               => FIFO_DATA_WIDTH,
    lpm_widthu              => FIFO_USEDW_BITS,
    overflow_checking       => "ON",
    underflow_checking      => "ON",
    use_eab                 => "ON"
  )
  port map
  (
    clock => Clock,
    usedw => open,
    empty => Read_Empty,
    full  => Write_Full,
    q     => fifo_Read_Data,
    wrreq => Write_Enable,
    aclr  => Reset,
    data  => fifo_Write_Data,
    rdreq => Read_Enable
  );
  
end architecture;
