# Synopsys Design Constraints (SDC) file for the Bayer demosaic core

# TODO - Define false paths, etc. to / from host registers and the pipeline

#
# False paths
# TODO - Create proper wildcard-based SDC statements for each:
#

# from */registers/core_Enable_Reg to */reset_Sync/pixel_Reset_Sync_d[*]

# We do want to incorporate -to target collections here. We *do* want
# the timing analyzer to check paths from the register to the host read
# datapath, and specifying only a -from would cull those paths as well.
#
# from */registers/column_Mode_Reg[*]
# from */registers/row_Mode_Reg[*]
#
# from */pipeline/window_Generator/write_Line_Toggle to */pipeline/window_Generator/write_Line_Toggle_d[1]

#
# Maximum-delay paths
# TODO - Create proper wildcard-based SDC statements for each:
#

# NOTE - Does Quartus Prime implement these properly now? In the past,
#        setting a max_delay constraint caused the placer to *try to
#        make the routes at least this long*, which is complete nonsense.
#
#        The intent here is to limit the length of routes from registers
#        being captured in the pixel clock domain to their capture in the
#        register file. The capture event occurs when the frame header
#        strobe is detected by a delay-line-based edge detector.
#
#        The length of the aforementioned delay line provides a guarantee:
#        that no less than an integer number of host clock periods (two less
#        than the delay line length, owing to domain-crossing uncertainty)
#        elapse between the pixel clock edge in which the last header field
#        is registered and the host clock edge in which the register is
#        sampled. This sampling is safe as long as the max_delay is satisfied.
#
#        If Altera fixed this, that is. ;)
#
# The max_delay value must be (3 * period(Host_Clock)). In the past, this
# has been difficult to directly compute from within the SDC... can this be
# done now, or can I auto-generate the SDC from the _hw.tcl?
#
# from */parser/frame_Resolution_Reg[*] to */registers/header_Frame_Res_Reg[*]
# from */parser/frame_Roi_Offset_Reg[*] to */registers/header_Frame_Roi_Reg[*]
# from */parser/frame_Index_Reg[*] to */registers/header_Frame_Index_Reg[*]
# from */parser/frame_Timestamp_Reg[*] to */registers/header_Frame_Timestamp_Reg[*]

# For Now -- just use false paths as we aren't going to have tight timing here...
set_false_path -from {*|parser*|frame_Resolution_Reg*[*]} -to {*|registers*|header_Frame_Res_Reg*[*]}
set_false_path -from {*|parser*|frame_Roi_Offset_Reg*[*]} -to {*|registers*|header_Frame_Roi_Reg*[*]}
set_false_path -from {*|parser*|frame_Index_Reg*[*]} -to {*|registers*|header_Frame_Index_Reg*[*]}
set_false_path -from {*|parser*|frame_Timestamp_Reg*[*]} -to {*|registers*|header_Frame_Timestamp_Reg*[*]}
set_false_path -from {*|registers*|*Mode_Reg} -to {*|window_Generator*|Output_*}
set_false_path -from {*|parser*|*frame_Header_Toggle} -to {*frame_Header_Toggle_d[*]}
set_false_path -from {*|bayer_demosaic*|*_Gain_Regs*} -to {*|bayer_demosaic*|*Products*}
set_false_path -from {*|bayer_demosaic*|core_Enable_Reg*} -to {*input_Reset_Sync_d*}

# from */pipeline/window_Generator/write_Line_Index[*] to */pipeline/window_Generator/window_Index[*] MAXDELAY (4 * OUTPUT_CLOCK_PERIOD)
# from */pipeline/window_Generator/write_Line_First to */pipeline/window_Generator/window_First MAXDELAY (4 * OUTPUT_CLOCK_PERIOD)

