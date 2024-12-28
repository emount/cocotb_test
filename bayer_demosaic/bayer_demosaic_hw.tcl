# File Name   : bayer_demosaic_hw.tcl
# Author      : Eldridge M. Mount IV
# Description : Wrapper TCL script for the Bayer_Demosaic core for use
#               in Intel Platform Designer.
#
#     o  0
#     | /       Copyright (c) 2017
#    (CL)---o   Critical Link, LLC
#      \
#       O

# +-----------------------------------
# | Request TCL package from ACDS 16.0
# | 
package require -exact qsys 16.0
# | 
# +-----------------------------------


# +-----------------------------------
# | Module bayer_demosaic
# | 
set_module_property DESCRIPTION                  "Bayer Demosaic Core"
set_module_property NAME                         bayer_demosaic
set_module_property VERSION                      1.0
set_module_property INTERNAL                     false
set_module_property OPAQUE_ADDRESS_MAP           true
set_module_property GROUP                        "Critical Link IP"
set_module_property AUTHOR                       "Eldridge M. Mount IV"
set_module_property ICON_PATH                    ./images/BayerPatternIcon.png
set_module_property DISPLAY_NAME                 bayer_demosaic
set_module_property INSTANTIATE_IN_SYSTEM_MODULE true
set_module_property EDITABLE                     true
set_module_property ELABORATION_CALLBACK         elaborate
set_module_property REPORT_TO_TALKBACK           false
set_module_property ALLOW_GREYBOX_GENERATION     false
set_module_property REPORT_HIERARCHY             false
# | 
# +-----------------------------------


# +-----------------------------------
# | Parameters for the module
# | 
add_parameter FPGA_FAMILY STRING ""
set_parameter_property FPGA_FAMILY DEFAULT_VALUE ""
set_parameter_property FPGA_FAMILY DISPLAY_NAME FPGA_FAMILY
set_parameter_property FPGA_FAMILY TYPE STRING
set_parameter_property FPGA_FAMILY UNITS None
set_parameter_property FPGA_FAMILY HDL_PARAMETER true
set_parameter_property FPGA_FAMILY SYSTEM_INFO_TYPE DEVICE_FAMILY

add_parameter PEL_DEPTH POSITIVE
set_parameter_property PEL_DEPTH DEFAULT_VALUE 16
set_parameter_property PEL_DEPTH DISPLAY_NAME PEL_DEPTH
set_parameter_property PEL_DEPTH TYPE POSITIVE
set_parameter_property PEL_DEPTH UNITS None
set_parameter_property PEL_DEPTH ALLOWED_RANGES 1:16
set_parameter_property PEL_DEPTH DESCRIPTION "Depth (dynamic range) of each pixel element, in bits"
set_parameter_property PEL_DEPTH AFFECTS_GENERATION false
set_parameter_property PEL_DEPTH HDL_PARAMETER true

add_parameter PIXELS_PER_CLOCK POSITIVE 16
set_parameter_property PIXELS_PER_CLOCK DEFAULT_VALUE 16
set_parameter_property PIXELS_PER_CLOCK DISPLAY_NAME PIXELS_PER_CLOCK
set_parameter_property PIXELS_PER_CLOCK TYPE POSITIVE
set_parameter_property PIXELS_PER_CLOCK UNITS None
set_parameter_property PIXELS_PER_CLOCK ALLOWED_RANGES {2 4 8 16}
set_parameter_property PIXELS_PER_CLOCK DESCRIPTION "Number of horizontal pixel values per clock"
set_parameter_property PIXELS_PER_CLOCK AFFECTS_GENERATION false
set_parameter_property PIXELS_PER_CLOCK HDL_PARAMETER true

add_parameter NUM_RGB_PLANES POSITIVE 3
set_parameter_property NUM_RGB_PLANES DEFAULT_VALUE 3
set_parameter_property NUM_RGB_PLANES DISPLAY_NAME NUM_RGB_PLANES
set_parameter_property NUM_RGB_PLANES TYPE POSITIVE
set_parameter_property NUM_RGB_PLANES VISIBLE false
set_parameter_property NUM_RGB_PLANES UNITS None
set_parameter_property NUM_RGB_PLANES ALLOWED_RANGES {3}
set_parameter_property NUM_RGB_PLANES AFFECTS_GENERATION false
set_parameter_property NUM_RGB_PLANES HDL_PARAMETER false

add_parameter PEL_LANE_BITS POSITIVE 16
set_parameter_property PEL_LANE_BITS DEFAULT_VALUE 16
set_parameter_property PEL_LANE_BITS DISPLAY_NAME PEL_LANE_BITS
set_parameter_property PEL_LANE_BITS TYPE POSITIVE
set_parameter_property PEL_LANE_BITS VISIBLE false
set_parameter_property PEL_LANE_BITS UNITS None
set_parameter_property PEL_LANE_BITS ALLOWED_RANGES {16}
set_parameter_property PEL_LANE_BITS AFFECTS_GENERATION false
set_parameter_property PEL_LANE_BITS HDL_PARAMETER false

add_parameter SUPPORTS_BACKPRESSURE INTEGER 0
set_parameter_property SUPPORTS_BACKPRESSURE DISPLAY_NAME SUPPORTS_BACKPRESSURE
set_parameter_property SUPPORTS_BACKPRESSURE ALLOWED_RANGES {0 1}
set_parameter_property SUPPORTS_BACKPRESSURE UNITS None
set_parameter_property SUPPORTS_BACKPRESSURE AFFECTS_GENERATION false
set_parameter_property SUPPORTS_BACKPRESSURE HDL_PARAMETER true

add_parameter EMPTY_SYMBOL_BITS_IN POSITIVE 4
set_parameter_property EMPTY_SYMBOL_BITS_IN DEFAULT_VALUE 4
set_parameter_property EMPTY_SYMBOL_BITS_IN DISPLAY_NAME EMPTY_SYMBOL_BITS_IN
set_parameter_property EMPTY_SYMBOL_BITS_IN DERIVED true
set_parameter_property EMPTY_SYMBOL_BITS_IN TYPE POSITIVE
set_parameter_property EMPTY_SYMBOL_BITS_IN VISIBLE false
set_parameter_property EMPTY_SYMBOL_BITS_IN UNITS None
set_parameter_property EMPTY_SYMBOL_BITS_IN AFFECTS_GENERATION false
set_parameter_property EMPTY_SYMBOL_BITS_IN HDL_PARAMETER true

add_parameter EMPTY_SYMBOL_BITS_OUT POSITIVE 4
set_parameter_property EMPTY_SYMBOL_BITS_OUT DEFAULT_VALUE 4
set_parameter_property EMPTY_SYMBOL_BITS_OUT DISPLAY_NAME EMPTY_SYMBOL_BITS_OUT
set_parameter_property EMPTY_SYMBOL_BITS_OUT DERIVED true
set_parameter_property EMPTY_SYMBOL_BITS_OUT TYPE POSITIVE
set_parameter_property EMPTY_SYMBOL_BITS_OUT VISIBLE false
set_parameter_property EMPTY_SYMBOL_BITS_OUT UNITS None
set_parameter_property EMPTY_SYMBOL_BITS_OUT AFFECTS_GENERATION false
set_parameter_property EMPTY_SYMBOL_BITS_OUT HDL_PARAMETER true

add_parameter MAX_HORIZ_RES POSITIVE 4096
set_parameter_property MAX_HORIZ_RES DEFAULT_VALUE 4096
set_parameter_property MAX_HORIZ_RES DISPLAY_NAME MAX_HORIZ_RES
set_parameter_property MAX_HORIZ_RES TYPE POSITIVE
set_parameter_property MAX_HORIZ_RES UNITS None
set_parameter_property MAX_HORIZ_RES ALLOWED_RANGES { 512 1024 2048 4096 8192 }
set_parameter_property MAX_HORIZ_RES AFFECTS_GENERATION false
set_parameter_property MAX_HORIZ_RES HDL_PARAMETER true

add_parameter INTERP_KERNEL_IDX NATURAL 1
set_parameter_property INTERP_KERNEL_IDX DEFAULT_VALUE 1
set_parameter_property INTERP_KERNEL_IDX DISPLAY_NAME INTERP_KERNEL_IDX
set_parameter_property INTERP_KERNEL_IDX TYPE NATURAL
set_parameter_property INTERP_KERNEL_IDX UNITS None
set_parameter_property INTERP_KERNEL_IDX ALLOWED_RANGES { 0:NearestNeighbor 1:Bilinear }
set_parameter_property INTERP_KERNEL_IDX AFFECTS_GENERATION false
set_parameter_property INTERP_KERNEL_IDX HDL_PARAMETER true

add_parameter USE_INPUT_EMPTY BOOLEAN FALSE
set_parameter_property USE_INPUT_EMPTY DEFAULT_VALUE FALSE
set_parameter_property USE_INPUT_EMPTY DISPLAY_NAME "Use Input Empty"
set_parameter_property USE_INPUT_EMPTY TYPE BOOLEAN
set_parameter_property USE_INPUT_EMPTY VISIBLE true
set_parameter_property USE_INPUT_EMPTY AFFECTS_GENERATION false
set_parameter_property USE_INPUT_EMPTY HDL_PARAMETER false

add_parameter USE_INPUT_READY BOOLEAN FALSE
set_parameter_property USE_INPUT_READY DEFAULT_VALUE FALSE
set_parameter_property USE_INPUT_READY DISPLAY_NAME "Support Backpressure"
set_parameter_property USE_INPUT_READY TYPE BOOLEAN
set_parameter_property USE_INPUT_READY VISIBLE true
set_parameter_property USE_INPUT_READY AFFECTS_GENERATION false
set_parameter_property USE_INPUT_READY HDL_PARAMETER false


# | 
# +-----------------------------------


# +-----------------------------------
# | Fileset for generation
# | 
# Define the synthesis fileset, associating the callback
add_fileset synthesis_fileset QUARTUS_SYNTH synthesis_callback

# Set the top-level entity name for the fileset
set_fileset_property synthesis_fileset TOP_LEVEL                     bayer_demosaic
set_fileset_property synthesis_fileset ENABLE_RELATIVE_INCLUDE_PATHS true
set_fileset_property synthesis_fileset ENABLE_FILE_OVERWRITE_MODE    false

# Bayer filter core files
add_fileset_file "Hdl_Utilities.vhd" VHDL PATH "../ip_common/hdl/Hdl_Utilities.vhd"
add_fileset_file "CL_Video_Defs.vhd" VHDL PATH "../ip_common/hdl/CL_Video_Defs.vhd"
add_fileset_file "Bayer_Demosaic_Kernels.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Kernels.vhd"
add_fileset_file "Bayer_Demosaic_Package.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Package.vhd"
add_fileset_file "Bayer_Demosaic_Registers.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Registers.vhd"
add_fileset_file "Bayer_Demosaic_Reset.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Reset.vhd"
add_fileset_file "Bayer_Demosaic_Parser.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Parser.vhd"
add_fileset_file "Bayer_Demosaic_Coords.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Coords.vhd"
add_fileset_file "Bayer_Demosaic_Line_Ram.vhd" VHDL PATH "./altera/Bayer_Demosaic_Line_Ram.vhd"
add_fileset_file "Bayer_Demosaic_Elastic_Fifo.vhd" VHDL PATH "./altera/Bayer_Demosaic_Elastic_Fifo.vhd"
add_fileset_file "Bayer_Demosaic_Buffers.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Buffers.vhd"
add_fileset_file "Bayer_Demosaic_Window.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Window.vhd"
add_fileset_file "Bayer_Bilinear_Kernel.vhd" VHDL PATH "./hdl/Bayer_Bilinear_Kernel.vhd"
add_fileset_file "Bayer_NN_Kernel.vhd" VHDL PATH "./hdl/Bayer_NN_Kernel.vhd"
add_fileset_file "Bayer_Arch_Selector_Kernel.vhd" VHDL PATH "./hdl/Bayer_Arch_Selector_Kernel.vhd"
add_fileset_file "Bayer_Demosaic_Balance.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Balance.vhd"
add_fileset_file "Bayer_Demosaic_Pipeline.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Pipeline.vhd"
add_fileset_file "Bayer_Demosaic_Elastic.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Elastic.vhd"
add_fileset_file "Bayer_Demosaic_Encapsulator.vhd" VHDL PATH "./hdl/Bayer_Demosaic_Encapsulator.vhd"
add_fileset_file "Bayer_Four_Pel_Interp.vhd" VHDL PATH "./hdl/Bayer_Four_Pel_Interp.vhd"
add_fileset_file "Bayer_Two_Pel_Interp.vhd" VHDL PATH "./hdl/Bayer_Two_Pel_Interp.vhd"
add_fileset_file "Bayer_Two_Pel_Adder.vhd" VHDL PATH "./hdl/Bayer_Two_Pel_Adder.vhd"

# Top-level wrapper HDL
add_fileset_file "bayer_demosaic.vhd" VHDL PATH "./bayer_demosaic.vhd"

# Module-level SDC file
add_fileset_file "bayer_demosaic.sdc" SDC PATH "./bayer_demosaic.sdc"
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Implement the synthesis callback, mostly as a stub for possible use
# | 
proc synthesis_callback {entityname} {
  if { [string compare ${entityname} "bayer_demosaic" ] != 0 } {
    # This check should never fail, just here to serve as a placeholder for a synthesis callback
    send_message error " Unexpected entity name $entityname; expected bayer_demosaic"
  }
}
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Host reset interface
# | 
add_interface Host_Reset reset end

set_interface_property Host_Reset associatedClock     Host_Clock
set_interface_property Host_Reset synchronousEdges    DEASSERT
set_interface_property Host_Reset ENABLED             true
set_interface_property Host_Reset EXPORT_OF           ""
set_interface_property Host_Reset PORT_NAME_MAP       ""
set_interface_property Host_Reset CMSIS_SVD_VARIABLES ""
set_interface_property Host_Reset SVD_ADDRESS_GROUP   ""

add_interface_port Host_Reset Host_Reset reset Input 1
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Host clock interface
# | 
add_interface Host_Clock clock end

set_interface_property Host_Clock clockRate           0
set_interface_property Host_Clock ENABLED             true
set_interface_property Host_Clock EXPORT_OF           ""
set_interface_property Host_Clock PORT_NAME_MAP       ""
set_interface_property Host_Clock CMSIS_SVD_VARIABLES ""
set_interface_property Host_Clock SVD_ADDRESS_GROUP   ""

add_interface_port Host_Clock Host_Clock clk Input 1
# | 
# +-----------------------------------


# +-----------------------------------
# | Host Avalon-MM interface
# | 
add_interface Host_Interface avalon end

set_interface_property Host_Interface addressUnits                    WORDS
set_interface_property Host_Interface associatedClock                 Host_Clock
set_interface_property Host_Interface associatedReset                 Host_Reset
set_interface_property Host_Interface bitsPerSymbol                   8
set_interface_property Host_Interface burstOnBurstBoundariesOnly      false
set_interface_property Host_Interface burstcountUnits                 WORDS
set_interface_property Host_Interface explicitAddressSpan             0
set_interface_property Host_Interface holdTime                        0
set_interface_property Host_Interface linewrapBursts                  false
set_interface_property Host_Interface maximumPendingReadTransactions  0
set_interface_property Host_Interface maximumPendingWriteTransactions 0
set_interface_property Host_Interface readLatency                     1
set_interface_property Host_Interface readWaitTime                    1
set_interface_property Host_Interface setupTime                       0
set_interface_property Host_Interface timingUnits                     Cycles
set_interface_property Host_Interface writeWaitTime                   0
set_interface_property Host_Interface ENABLED                         true
set_interface_property Host_Interface EXPORT_OF                       ""
set_interface_property Host_Interface PORT_NAME_MAP                   ""
set_interface_property Host_Interface CMSIS_SVD_VARIABLES             ""
set_interface_property Host_Interface SVD_ADDRESS_GROUP               ""

add_interface_port Host_Interface Host_Address    address    Input   8
add_interface_port Host_Interface Host_Write      write      Input   1
add_interface_port Host_Interface Host_WriteData  writedata  Input  32
add_interface_port Host_Interface Host_ByteEnable byteenable Input   4
add_interface_port Host_Interface Host_ReadData   readdata   Output 32
add_interface_port Host_Interface Host_Read       read       Input   1

set_interface_assignment Host_Interface embeddedsw.configuration.isFlash              0
set_interface_assignment Host_Interface embeddedsw.configuration.isMemoryDevice       0
set_interface_assignment Host_Interface embeddedsw.configuration.isNonVolatileStorage 0
set_interface_assignment Host_Interface embeddedsw.configuration.isPrintableDevice    0
# | 
# +-----------------------------------


# +-----------------------------------
# | Host IRQ interface
# | 
add_interface Host_Irq interrupt end

set_interface_property Host_Irq associatedAddressablePoint Host_Interface
set_interface_property Host_Irq associatedClock            Host_Clock
set_interface_property Host_Irq associatedReset            Host_Reset
set_interface_property Host_Irq bridgedReceiverOffset      ""
set_interface_property Host_Irq bridgesToReceiver          ""
set_interface_property Host_Irq ENABLED                    true
set_interface_property Host_Irq EXPORT_OF                  ""
set_interface_property Host_Irq PORT_NAME_MAP              ""
set_interface_property Host_Irq CMSIS_SVD_VARIABLES        ""
set_interface_property Host_Irq SVD_ADDRESS_GROUP          ""

add_interface_port Host_Irq Host_Interrupt irq Output 1
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Input reset interface
# | 
add_interface Input_Reset reset end

set_interface_property Input_Reset associatedClock     Input_Clock
set_interface_property Input_Reset synchronousEdges    DEASSERT
set_interface_property Input_Reset ENABLED             true
set_interface_property Input_Reset EXPORT_OF           ""
set_interface_property Input_Reset PORT_NAME_MAP       ""
set_interface_property Input_Reset CMSIS_SVD_VARIABLES ""
set_interface_property Input_Reset SVD_ADDRESS_GROUP   ""

add_interface_port Input_Reset Input_Reset reset Input 1
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Input clock interface
# | 
add_interface Input_Clock clock end

# TODO - Determine the input clock rate dynamically
set_interface_property Input_Clock clockRate           0
set_interface_property Input_Clock ENABLED             true
set_interface_property Input_Clock EXPORT_OF           ""
set_interface_property Input_Clock PORT_NAME_MAP       ""
set_interface_property Input_Clock CMSIS_SVD_VARIABLES ""
set_interface_property Input_Clock SVD_ADDRESS_GROUP   ""

add_interface_port Input_Clock Input_Clock clk Input 1
# | 
# +-----------------------------------


# +-----------------------------------
# | Input Avalon-ST video interface
# | 
add_interface Input_Pel_Stream avalon_streaming end
set_interface_property Input_Pel_Stream associatedClock            Input_Clock
set_interface_property Input_Pel_Stream associatedReset            Input_Reset
set_interface_property Input_Pel_Stream errorDescriptor            ""
set_interface_property Input_Pel_Stream firstSymbolInHighOrderBits true
set_interface_property Input_Pel_Stream maxChannel                 0
set_interface_property Input_Pel_Stream readyLatency               0
set_interface_property Input_Pel_Stream ENABLED                    true
set_interface_property Input_Pel_Stream EXPORT_OF                  ""
set_interface_property Input_Pel_Stream PORT_NAME_MAP              ""
set_interface_property Input_Pel_Stream CMSIS_SVD_VARIABLES        ""
set_interface_property Input_Pel_Stream SVD_ADDRESS_GROUP          ""

add_interface_port Input_Pel_Stream Input_Valid         valid         Input  1
add_interface_port Input_Pel_Stream Input_StartOfPacket startofpacket Input  1
add_interface_port Input_Pel_Stream Input_EndOfPacket   endofpacket   Input  1
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Output reset interface
# | 
add_interface Output_Reset reset end

set_interface_property Output_Reset associatedClock     Output_Clock
set_interface_property Output_Reset synchronousEdges    DEASSERT
set_interface_property Output_Reset ENABLED             true
set_interface_property Output_Reset EXPORT_OF           ""
set_interface_property Output_Reset PORT_NAME_MAP       ""
set_interface_property Output_Reset CMSIS_SVD_VARIABLES ""
set_interface_property Output_Reset SVD_ADDRESS_GROUP   ""

add_interface_port Output_Reset Output_Reset reset Input 1
# | 
# +-----------------------------------
  

# +-----------------------------------
# | Output clock interface
# | 
add_interface Output_Clock clock end

# TODO - Determine the output clock rate dynamically
set_interface_property Output_Clock clockRate           0
set_interface_property Output_Clock ENABLED             true
set_interface_property Output_Clock EXPORT_OF           ""
set_interface_property Output_Clock PORT_NAME_MAP       ""
set_interface_property Output_Clock CMSIS_SVD_VARIABLES ""
set_interface_property Output_Clock SVD_ADDRESS_GROUP   ""

add_interface_port Output_Clock Output_Clock clk Input 1
# | 
# +-----------------------------------


# +-----------------------------------
# | Output Avalon-ST video interface
# | 
add_interface Output_Pel_Stream avalon_streaming start
set_interface_property Output_Pel_Stream associatedClock            Output_Clock
set_interface_property Output_Pel_Stream associatedReset            Output_Reset
set_interface_property Output_Pel_Stream errorDescriptor            ""
set_interface_property Output_Pel_Stream firstSymbolInHighOrderBits true
set_interface_property Output_Pel_Stream maxChannel                 0
set_interface_property Output_Pel_Stream readyLatency               0
set_interface_property Output_Pel_Stream ENABLED                    true
set_interface_property Output_Pel_Stream EXPORT_OF                  ""
set_interface_property Output_Pel_Stream PORT_NAME_MAP              ""
set_interface_property Output_Pel_Stream CMSIS_SVD_VARIABLES        ""
set_interface_property Output_Pel_Stream SVD_ADDRESS_GROUP          ""

add_interface_port Output_Pel_Stream Output_Valid         valid         Output 1
add_interface_port Output_Pel_Stream Output_StartOfPacket startofpacket Output 1
add_interface_port Output_Pel_Stream Output_EndOfPacket   endofpacket   Output 1
# | 
# +-----------------------------------


# +-----------------------------------
# | Elaboration callback method
# | 
proc log2 x "expr {log(\$x)/[expr log(2)]}"
# | 
# +-----------------------------------

# +-----------------------------------
# | Elaboration callback method
# | 
proc elaborate {} {
    # Set derived parameter values based upon their precursor variables
    set pel_depth [get_parameter_value PEL_DEPTH]
    set pix_per_clk [get_parameter_value PIXELS_PER_CLOCK]
    set num_rgb_planes [get_parameter_value NUM_RGB_PLANES]
    set pel_lane_bits [get_parameter_value PEL_LANE_BITS]
    set use_input_empty [get_parameter_value USE_INPUT_EMPTY]
    set use_input_ready [get_parameter_value USE_INPUT_READY]
    

    # The empty vectors for input and output are of the same dimension;
    # each has a respective number of bits per symbol which normalizes
    # this quantity to the same dimension.

    # The input stream has a single, Bayer-subsampled image plane
    #
    # I know this is ugly, but right now CL's IP expects 8 bits per symbol.  This was 
    # primarily to deal with RAM interfaces (which required 8 bits per symbol for integration).
    # This is totally wrong and we should be using the PEL_LANE_BITS factor for symbols, but that will
    # require reworking a bunch of other IP that I don't want to touch right now.
    # Hopefully we don't have endian issues here...
    # set_interface_property Input_Pel_Stream dataBitsPerSymbol [get_parameter_value PEL_LANE_BITS]
    set_interface_property Input_Pel_Stream dataBitsPerSymbol 8
    add_interface_port Input_Pel_Stream Input_Data data Input [expr (${pix_per_clk} * ${pel_lane_bits})]
    
    if { ${use_input_empty} } {
        add_interface_port Input_Pel_Stream Input_Empty empty Input [expr int(ceil([expr {log((${pix_per_clk} * ${pel_lane_bits})/8) / [expr log(2)]}]))]
    }
    
    if { ${use_input_ready} } {
        add_interface_port Input_Pel_Stream Input_Ready         ready         Output 1
        # if input backpressure is supported, then output backpressure is supported
        add_interface_port Output_Pel_Stream Output_Ready         ready         Input  1
    }

	set_parameter_value EMPTY_SYMBOL_BITS_IN [expr int(ceil([expr {log((${pix_per_clk} * ${pel_lane_bits})/8) / [expr log(2)]}]))]

    # The output stream has RGB color planes per pixel beat
    # See comment above regarding input_pel_stream databitspersymbol
    set_interface_property Output_Pel_Stream dataBitsPerSymbol 8
    add_interface_port Output_Pel_Stream Output_Data data Output [expr (${pix_per_clk} * ${num_rgb_planes} * ${pel_lane_bits})]
    add_interface_port Output_Pel_Stream Output_Empty empty Output [expr int(ceil([expr {log((${pix_per_clk}* ${num_rgb_planes} * ${pel_lane_bits})/8) / [expr log(2)]}]))]

    set_parameter_value EMPTY_SYMBOL_BITS_OUT [expr int(ceil([expr {log((${pix_per_clk}* ${num_rgb_planes} * ${pel_lane_bits})/8) / [expr log(2)]}]))]

}
# | 
# +-----------------------------------
