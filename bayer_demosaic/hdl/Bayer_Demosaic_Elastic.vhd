-- File Name   : Bayer_Demosaic_Elastic.vhd
-- Author      : Eldridge M. Mount IV
-- Description : This submodule serves to provide a small amount of temporal
--               "elastic", i.e. a FIFO buffer, for absorbing pipeline data
--               during cycles in which the output interface is backpressured.
--
--     o  0
--     | /       Copyright (c) 2017-2018
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

-- Utilities package
library work;
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Elastic is
  generic
  (
    -- FPGA_FAMILY    - String indicating the FPGA family to compile for
    -- DATA_WIDTH     - Width of the input and output data vectors, in bits
    -- BYPASS_ELASTIC - Flag indicating whether to bypass backpressure logic
    FPGA_FAMILY    : string;
    DATA_WIDTH     : positive;
    BYPASS_ELASTIC : boolean
  );
  port
  (
    -- Input reset and clock
    Pixel_Reset : in std_logic;
    Pixel_Clock : in std_logic;

    -- Input pixel stream
    Input_Valid : in  std_logic;
    Input_Start : in  std_logic;
    Input_End   : in  std_logic;
    Input_Data  : in  std_logic_vector((DATA_WIDTH - 1) downto 0);
    Input_Ready : out std_logic;

    -- Absorbed output stream
    Output_Valid : out std_logic;
    Output_Start : out std_logic;
    Output_End   : out std_logic;
    Output_Data  : out std_logic_vector((DATA_WIDTH - 1) downto 0);
    Output_Ready : in  std_logic
  );
end entity;


-- Architecture implementation
architecture Rtl of Bayer_Demosaic_Elastic is
begin

  --
  -- Select between bypass and the elastic logic based on configuration
  --
  select_Elastic : if(BYPASS_ELASTIC) generate

    --
    -- Always indicate readiness for data, and ignore backpressure entirely
    --
    Input_Ready  <= '1';
    Output_Valid <= Input_Valid;
    Output_Start <= Input_Start;
    Output_End   <= Input_End;
    Output_Data  <= Input_Data;

  else generate

    --
    -- Constants and signals for the wrapped FIFO
    --
    -- The depth is automatically computed in an effort to target the smallest-
    -- density block RAMs supported by devices likely to be targeted with this
    -- IP core (Intel / Altera M10K block RAMs). Parity bits will actually be
    -- used for start / end flags, but aren't involved in this computation.
    --
    -- The data width is rounded up to the nearest integer power of two to ensure
    -- only power-of-two depths are requested of the FIFO core.
    --
    constant TARGET_RAM_KIB  : positive := 8;
    constant CEIL_DATA_WIDTH : positive := (2 ** Min_Repr_Bits(DATA_WIDTH - 1));
    constant FIFO_DEPTH      : positive := ((TARGET_RAM_KIB * 1_024) / CEIL_DATA_WIDTH);

    -- Elastic logic state space
    constant FIFO_SETTLE_CLOCKS : positive := 8;
    constant SETTLE_COUNT_BITS  : positive := Min_Repr_Bits(FIFO_SETTLE_CLOCKS - 1);
    signal settle_Counter_Reset : std_logic;
    signal settle_Counter       : unsigned((SETTLE_COUNT_BITS - 1) downto 0) := To_Unsigned(0, SETTLE_COUNT_BITS);
    signal settle_Counter_Tc    : std_logic := '0';
    type Elastic_State_Type is (ELASTIC_RESET, ELASTIC_SETTLE, ELASTIC_ACTIVE);
    signal next_Fifo_State    : Elastic_State_Type;
    signal present_Fifo_State : Elastic_State_Type;
    signal fifo_Reset         : std_logic;
    signal fifo_Write         : std_logic;
    signal fifo_Full          : std_logic;
    signal fifo_Read          : std_logic;
    signal fifo_Empty         : std_logic;
    signal input_Ready_int    : std_logic;

    begin

      --
      -- Instantiate the wrapped FIFO primitive
      --
      -- This FIFO implementing the memory required to store
      -- incoming data prior to for storage of data
      --
      elastic_Fifo : entity work.Bayer_Demosaic_Elastic_Fifo
      generic map
      (
        FPGA_FAMILY => FPGA_FAMILY,
        DATA_WIDTH  => DATA_WIDTH,
        FIFO_DEPTH  => FIFO_DEPTH
      )
      port map
      (
        -- Common clock and synchronous reset
        Reset => fifo_Reset,
        Clock => Pixel_Clock,
        
        -- Write interface
        Write_Enable => fifo_Write,
        Write_Start  => Input_Start,
        Write_End    => Input_End,
        Write_Data   => Input_Data,
        Write_Full   => fifo_Full,
    
        -- Read interface
        Read_Enable => fifo_Read,
        Read_Start  => Output_Start,
        Read_End    => Output_End,
        Read_Data   => Output_Data,
        Read_Empty  => fifo_Empty
      );


      --
      -- State machine governing the elastic FIFO
      --
      govern_Elastic_Comb : process(all)
      begin
      
        -- Tend to remain in the present state
        next_Fifo_State <= present_Fifo_State;
      
        -- Default Mealy outputs
        fifo_Read            <= '0';
        fifo_Write           <= '0';
        settle_Counter_Reset <= '1';
      
        -- Next-state excitation
        case present_Fifo_State is
          when ELASTIC_RESET =>
            -- Place the FIFO in reset until the reset hold expires; the settle
            -- count is re-used, it suffices.
            --
            -- Of note is the fact that incoming valid data is not gated through
            -- to the write signal - many FIFOs cannot reliably reset in the presence
            -- of active control signals. Vendor behavioral models will generally
            -- indicate this with undefined ('X') outputs if minimum reset hold
            -- and deassertion-settling requirements are not met by client logic.
            --
            -- This may not be true of the synchronous FIFO used here, but it
            -- doesn't consume very much logic and is good insurance for portability.
            if ?? settle_Counter_Tc then
              -- Change states and permit the settle counter to be reset
              next_Fifo_State <= ELASTIC_SETTLE;
            else
              -- Let the settle counter run to measure the reset hold interval
              settle_Counter_Reset <= '0';
            end if;
      
          when ELASTIC_SETTLE =>
            -- De-assert the FIFO reset but continue to prevent any control signal
            -- activity until the minimum settling period has elapsed
            if ?? settle_Counter_Tc then
              next_Fifo_State <= ELASTIC_ACTIVE;
            else
              settle_Counter_Reset <= '0';
            end if;
      
          when ELASTIC_ACTIVE =>
            -- The FIFO is now permitted to become active for writes and reads;
            -- pass valid data along as writes without any inspection.
            fifo_Write <= Input_Valid;

            -- Reads occur when the downstream sink is ready and the FIFO has data
            fifo_Read <= (Output_Ready and not fifo_Empty);
            
            -- Remain in this state, implementing backpressure unless an overflow
            -- error occurs. The 'full' state is taken as good enough as an overflow
            -- for sake of logic efficiency.
            if ?? fifo_Full then
              next_Fifo_State <= ELASTIC_RESET;
            end if;
      
        end case;
      end process;
      
      
      govern_Elastic_Seq : process(Pixel_Reset, Pixel_Clock)
      begin
        if ?? Pixel_Reset then
          present_Fifo_State <= ELASTIC_RESET;
          fifo_Reset         <= '1';
          Output_Valid       <= '0';
        elsif rising_edge(Pixel_Clock) then
      
          -- Update the FIFO state
          present_Fifo_State <= next_Fifo_State;

          -- Default Moore outputs
          Output_Valid <= '0';
      
          -- Present the input ready as a pipelined replica of that observed from
          -- the downstream sink
          input_Ready_int <= Output_Ready;
          
          -- Registered output excitation
          case present_Fifo_State is
            when ELASTIC_RESET =>
              -- Continue holding the FIFO in reset for a cycle after the transition
              -- out of this state. This register avoids a long path around the FIFO.
              fifo_Reset <= '1';

            when ELASTIC_ACTIVE =>
              -- Reads from the FIFO have a ready latency of one cycle
              Output_Valid <= fifo_Read;
      
            when others =>
              -- De-assert reset in all other states
              fifo_Reset <= '0';
              
          end case;

        end if;
      end process;


      --
      -- Operate a simple synchronous counter for hold / settling intervals
      --
      counter_Logic : process(Pixel_Clock)
      begin
        if rising_edge(Pixel_Clock) then
          if ?? settle_Counter_Reset then
            settle_Counter     <= To_Unsigned(0, settle_Counter'LENGTH);
            settle_Counter_Tc  <= '0';
          elsif(settle_Counter_Tc = '0') then
            settle_Counter <= (settle_Counter + 1);
            if(settle_Counter = (FIFO_SETTLE_CLOCKS - 2)) then
              settle_Counter_Tc <= '1';
            end if;
          end if;
        end if;
      end process;

      
      --
      -- Drive the input readiness indication to the upstream source
      --
      Input_Ready <= input_Ready_int;
      
    end; -- Generate declarative block

  end generate;
  
end architecture;
