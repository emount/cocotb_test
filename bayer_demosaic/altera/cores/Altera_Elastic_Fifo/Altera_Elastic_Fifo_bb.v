
module Altera_Elastic_Fifo (
	data,
	wrreq,
	rdreq,
	clock,
	sclr,
	q,
	usedw,
	full,
	empty);	

	input	[257:0]	data;
	input		wrreq;
	input		rdreq;
	input		clock;
	input		sclr;
	output	[257:0]	q;
	output	[4:0]	usedw;
	output		full;
	output		empty;
endmodule
