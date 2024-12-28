#!/bin/sh
#
# File        : smoketest.sh
# Author      : Eldridge M. Mount IV
# Description : "Smoke test" script to run GHDL validation tests as well as a simple
#               GHDL + cocotb example testbench. Successful execution of both is a
#               validation of a correctly-built container.
# 
# COPYRIGHT (c) 2017 MKS Instruments, Inc. All rights reserved.
#
# NOTICE: Contains confidential and proprietary information of MKS Instruments Inc.
#         and is protected under the copyright laws as an unpublished work.

# First, run the GHDL test suite
pushd .; cd ghdl/testsuite; GHDL=ghdl ./testsuite.sh; popd

# Next, run a cocotb example testbench with GHDL as the simulator
cd cocotb/examples/endian_swapper/tests; TOPLEVEL_LANG=vhdl SIM=ghdl make
