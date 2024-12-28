# File        : Makefile
# Author      : Eldridge M. Mount IV
# Description : Makefile for cocotb-based functional verification of
#               the Bayer_Demosaic Qsys core.
#
#     o  0
#     | /       Copyright (c) 2017-2018
#    (CL)---o   Critical Link, LLC
#      \
#       O

# The core is implemented in VHDL, and we are using GHDL / GTKWave
# as a simulation environment.
TOPLEVEL_LANG = vhdl
SIM = ghdl

# Simulator-specific arguments to pass
GHDL_ARGS = --std=08

# Argument to request inclusion of non-standard packages nevertheless
# required by some vendor packages (std_logic_arith, std_logic_unsigned, etc.)
GHDL_ARGS += --ieee=synopsys

# Compiler optimization level
GHDL_ARGS += -O3

# Begin simulation arguments
SIM_ARGS = $(GHDL_ARGS)

# Variables defining some basic commands
PWD = $(shell pwd)

# Available kernels for Bayer interpolation
INTERP_KERNEL_NEAREST  = 0
INTERP_KERNEL_BILINEAR = 1

# Default values for top-level DUT generics
FPGA_FAMILY           ?= "Arria V"
PEL_DEPTH             ?= 16
PIX_PER_CLK           ?= 16
SUPPORTS_BACKPRESSURE ?= 0 # Set to 1 once backpressure is working
INTERP_KERNEL_IDX     ?= $(INTERP_KERNEL_BILINEAR)
MAX_HORIZ_RES         ?= 4096

# Set up the waveform
WAVE_FILE   = $(PWD)/bayer_demosaic.ghw
WAVE_OPTS   = $(PWD)/wave.opts
WAVE_LAYOUT = $(PWD)/wave-layouts/bayer_demosaic-$(PIX_PER_CLK)pix.gtkw

# Either write a full wave option file or read existing
WAVE_ACTION = write
# WAVE_ACTION = read

# Simulator arguments
SIM_ARGS = --wave=$(WAVE_FILE) --$(WAVE_ACTION)-wave-opt=$(WAVE_OPTS)

# Path assignments for use
CORE_PATH = $(PWD)/..
IP_COMMON = $(CORE_PATH)/../ip_common

# Conditionally assign the COCOTB installation path if it is not
# already provided by the environment. This relative location is
# appropriate for when the cocotb-1.0 Docker container is used,
# with a top-level bind mount of the module subdirectory.
COCOTB ?= $(CORE_PATH)/../cocotb

# Similarly, provide a default for the GHDL and vendor library source
# installation paths, which are needed for vendor library compilation
# scripts
GHDL_PATH  ?= $(CORE_PATH)/../ghdl
INTEL_PATH ?= $(CORE_PATH)/../intel_libs

# Construct the Python path to incorporate :
#
# * Common CL IP infrastructure for Cocotb
# * DUT-specific behavioral modeling modules
PYTHONPATH := $(IP_COMMON):$(CORE_PATH)/model:$(PYTHONPATH)

# Define the design sources
COMMON_HDL    = $(IP_COMMON)/hdl
HDL_PATH      = $(CORE_PATH)/hdl
IP_PATH       = $(CORE_PATH)/altera
VHDL_SOURCES  = $(COMMON_HDL)/Hdl_Utilities.vhd
VHDL_SOURCES += $(COMMON_HDL)/CL_Video_Defs.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Kernels.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Package.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Registers.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Reset.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Parser.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Coords.vhd
VHDL_SOURCES += $(IP_PATH)/Bayer_Demosaic_Elastic_Fifo.vhd
VHDL_SOURCES += $(IP_PATH)/Bayer_Demosaic_Line_Ram.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Buffers.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Window.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Two_Pel_Adder.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Two_Pel_Interp.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Four_Pel_Interp.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Bilinear_Kernel.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_NN_Kernel.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Arch_Selector_Kernel.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Balance.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Pipeline.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Elastic.vhd
VHDL_SOURCES += $(HDL_PATH)/Bayer_Demosaic_Encapsulator.vhd
VHDL_SOURCES += $(CORE_PATH)/bayer_demosaic.vhd

# Include dynamically-generated source files
VHDL_SOURCES += $(PWD)/sim_wrapper.vhd

# Define the top-level HDL entity and the Python test module,
# respectively
TOPLEVEL := sim_wrapper
MODULE   := test_bayer_demosaic

# Add a custom pre-simulation target before core cocotb
# Makefile inclusion
CUSTOM_SIM_DEPS += presim

# Create a dependency of the simulation target upon the custom
# pre-simulation target for this Makefile
#
# TODO - Update Cocotb / GHDL and incorporate this into a patch
#        for cocotb/makefiles/simulators/Makefile.sim
sim: presim

# Include the design-independent core cocotb Makefiles
include $(COCOTB)/makefiles/Makefile.inc
include $(COCOTB)/makefiles/Makefile.sim

# Definitions and target for generating a top-level simulation wrapper
# for parameterization of DUT generics for a set of tests
TOPLEVEL_HDL = $(TOPLEVEL).vhd
.PHONY: $(TOPLEVEL_HDL)
$(TOPLEVEL_HDL):
	./gen_sim_wrapper.sh $(PEL_DEPTH) $(PIX_PER_CLK) $(FPGA_FAMILY) $(INTERP_KERNEL_IDX) $(SUPPORTS_BACKPRESSURE) $(MAX_HORIZ_RES)

# Pre-simulation target for simulation wrapper generation and cleanup.
#
# TODO - How can we map directly to the $(INTEL_PATH) directory in
#        GHDL? All the libraries are already built there. Having to
#        copy them into $(SIM_BUILD) is a hack, and symlinks do not
#        get mapped by GHDL.
.PHONY: presim
presim: sim_wrapper.vhd
	mkdir -p $(SIM_BUILD)
	rm -rf $(SIM_BUILD)/altera       ; cp -R $(INTEL_PATH)/altera       $(SIM_BUILD)/
	rm -rf $(SIM_BUILD)/altera_lnsim ; cp -R $(INTEL_PATH)/altera_lnsim $(SIM_BUILD)/
	rm -rf $(SIM_BUILD)/altera_mf    ; cp -R $(INTEL_PATH)/altera_mf    $(SIM_BUILD)/
	rm -rf $(SIM_BUILD)/lpm          ; cp -R $(INTEL_PATH)/lpm          $(SIM_BUILD)/
	rm -rf $(SIM_BUILD)/sgate        ; cp -R $(INTEL_PATH)/sgate        $(SIM_BUILD)/

# Remove any existing waveform options file before simulation, since
# GHDL doesn't seem to have an option to force overwrite.
ifeq ($(WAVE_ACTION),write)
	rm -f $(WAVE_OPTS)
endif


# Target to bring up the default waveform display
.PHONY: wave
wave:
	gtkwave --save=$(WAVE_LAYOUT) --dump=$(WAVE_FILE)&
