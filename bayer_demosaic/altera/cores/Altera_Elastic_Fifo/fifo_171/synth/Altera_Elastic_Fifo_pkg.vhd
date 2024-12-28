library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

package Altera_Elastic_Fifo_pkg is
	component Altera_Elastic_Fifo_fifo_171_fjuzqtq is
		port (
			data  : in  std_logic_vector(257 downto 0) := (others => 'X'); -- datain
			wrreq : in  std_logic                      := 'X';             -- wrreq
			rdreq : in  std_logic                      := 'X';             -- rdreq
			clock : in  std_logic                      := 'X';             -- clk
			sclr  : in  std_logic                      := 'X';             -- sclr
			q     : out std_logic_vector(257 downto 0);                    -- dataout
			usedw : out std_logic_vector(4 downto 0);                      -- usedw
			full  : out std_logic;                                         -- full
			empty : out std_logic                                          -- empty
		);
	end component Altera_Elastic_Fifo_fifo_171_fjuzqtq;

end Altera_Elastic_Fifo_pkg;
