-- File Name   : Bayer_Demosaic_Reset.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Module implementing reset logic for the Bayer_Demosaic
--               IP core.
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


-- Entity declaration
entity Bayer_Demosaic_Reset is
  port
  (
    -- Input reset and clock 
    Input_Reset : in std_logic;
    Input_Clock : in std_logic;
   
    -- Output reset and clock 
    Output_Reset : in std_logic;
    Output_Clock : in std_logic;
   
    -- Host control interface
    Core_Enable : in std_logic;

    -- Synchronized reset outputs
    Input_Reset_Sync  : out std_logic;
    Output_Reset_Sync : out std_logic
  );
end entity;


-- Architecture definition
architecture rtl of Bayer_Demosaic_Reset is

  -- Reset synchronization signals
  constant RESET_SYNC_TAPS   : positive := 4;
  signal input_Reset_Comb    : std_logic;
  signal input_Reset_Sync_d  : std_logic_vector(1 to RESET_SYNC_TAPS) := (others => '1');
  signal output_Reset_Comb   : std_logic;
  signal output_Reset_Sync_d : std_logic_vector(1 to RESET_SYNC_TAPS) := (others => '1');

begin

  --
  -- Processes to locally-synchronize a reset for each pixel clock domain.
  -- Either assertion of the interface reset or deassertion of the host-
  -- controlled Core_Enable signal places the pipeline into reset with a
  -- clock-synchronized, controlled-width signal.
  --

  -- Input pixel clock domain
  input_Reset_Comb <= (Input_Reset or not Core_Enable);
  sync_Input_Reset : process(input_Reset_Comb, Input_Clock)
  begin
    if ?? input_Reset_Comb then
      input_Reset_Sync_d <= (others => '1');
    elsif rising_edge(Input_Clock) then
      input_Reset_Sync_d <= ('0' & input_Reset_Sync_d(1 to (input_Reset_Sync_d'HIGH - 1)));
    end if;
  end process;

  -- Drive the synchronized reset tap out
  Input_Reset_Sync <= input_Reset_Sync_d(input_Reset_Sync_d'HIGH);

  
  -- Output pixel clock domain
  output_Reset_Comb <= (Output_Reset or not Core_Enable);
  sync_Output_Reset : process(output_Reset_Comb, Output_Clock)
  begin
    if ?? output_Reset_Comb then
      output_Reset_Sync_d <= (others => '1');
    elsif rising_edge(Output_Clock) then
      output_Reset_Sync_d <= ('0' & output_Reset_Sync_d(1 to (output_Reset_Sync_d'HIGH - 1)));
    end if;
  end process;

  -- Drive the synchronized reset tap out
  Output_Reset_Sync <= output_Reset_Sync_d(output_Reset_Sync_d'HIGH);

end architecture;
