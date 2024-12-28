"""
File        : test_bayer_demosaic.py
Author      : Eldridge M. Mount IV
Description : Cocotb top-level test / testbench module for the
              Bayer Demosaic FPGA IP core.

              The specific application of the underlying IP core is
              a wrapper layer designed for use within an Intel (Altera)
              Platform Designer system-level design.

              Test cases are run for all permutations of the pixels-per-
              clock and other parameters.

     o  0
     | /       Copyright (c) 2017-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
from bayer_demosaic import BayerDemosaic

import cl_coco
from cl_coco.imaging.image_defs import RGBPlanes
from cl_coco.imaging.patterns import MonoGradient, PatternGenerator, RgbGradient, UniformRandom
from cl_coco.imaging.transform import BayerPattern
from cl_coco.video.cl_video_defs import VideoFormats, VideoFramePacking

import cocotb
from cocotb.generators.bit import wave, intermittent_single_cycles, random_50_percent
from cocotb.regression import TestFactory
from cocotb.result import TestFailure
from cocotb.triggers import Timer
from cocotb.utils import get_sim_steps


# Constants shared by tests
INPUT_CLOCK_MHZ = 100.0
OUTPUT_CLOCK_MHZ =125.0
HOST_CLOCK_MHZ = 50.0
RESET_USEC = 1.0
DELAY_END_USEC = 10.0


@cocotb.coroutine
def bayer_test(dut,
               video_format=None,
               same_clocks=None,
               idle_inserter=None,
               sink_backpressure=None,
               pattern_generator=None,
               bayer_row_mode=None,
               bayer_column_mode=None):
    """Core test coroutine

    See the test factory options at the bottom of the module for an
    understanding of the keyword args passed to this coroutine.
    """
    log = cocotb.logging.getLogger("cocotb.test")
    log.info("Running Bayer Demosaic core test")

    # Sanity-check a few input parameters
    if(OUTPUT_CLOCK_MHZ < INPUT_CLOCK_MHZ):
        raise TestFailure("Output clock ({} MHz) must be as fast or faster than input clock ({} MHz)".format(INPUT_CLOCK_MHZ, OUTPUT_CLOCK_MHZ))
    
    # Fixed test parameters
    video_params = VideoFramePacking.VID_PARAMS_RGB24
    num_frames = 1

    # Create the model abstracting the DUT and its testbench
    demosaic = BayerDemosaic(dut, log)

    # Initialize the test
    yield demosaic.init_test(same_clocks,
                             INPUT_CLOCK_MHZ,
                             OUTPUT_CLOCK_MHZ,
                             HOST_CLOCK_MHZ,
                             RESET_USEC,
                             sink_backpressure,
                             pattern_generator,
                             bayer_row_mode,
                             bayer_column_mode,
                             video_format,
                             video_params,
                             num_frames,
                             idle_inserter,
                             log=log)

    # Delay for a bit to let the test settle
    yield Timer(get_sim_steps(DELAY_END_USEC, 'us'))

    # Consult the scoreboard for the final determination of pass / fail
    raise demosaic.scoreboard.result


#
# Test factory for the Bayer demosaic simulation
#
# Set the variable 'do_regression' to enable or disable permuting of
# options. If disabled, a set of options is chosen which are reasonable
# for logic development.
#
# NOTE - The top-level entity is also permuted with options for
#        pel depth, pixels per clock, etc. Options here are thereby
#        implicitly permuted through the entire test coverage space.
#
# TODO - Provide some locally-randomized modulation of the generative
#        distributions underlying idle generation.
#
do_regression = False
do_full_frame = False
factory = TestFactory(bayer_test)


#
# Video stimulus format
#
format_options = [VideoFormats.VID_FMT_QCIF] # VESA_XGA] # QCIF] # TINY]
if do_regression:
    format_options.append(VideoFormats.VID_FMT_QCIF)
    
if do_full_frame:
    format_options.append(VideoFormats.VID_FMT_VESA_XGA)

factory.add_option('video_format', format_options)


#
# Relationship of the input and output clocks
#
same_clocks_options = [True]
if do_regression:
    same_clocks_options.append(False)

factory.add_option('same_clocks', same_clocks_options)


#
# Generation of idle cycles on the input stream
#
idle_options = [None]
if do_regression:
    idle_options.extend([intermittent_single_cycles, wave, random_50_percent])

factory.add_option('idle_inserter', idle_options)


#
# Generation of backpressure from the output sink
#
# Each option is represented as a tuple of (likelihood %, max_delay)
# for the sink to decide to assert 'not ready' after accepting an active
# beat of data at the output stream.
#
backpressure_options = [(0.0, 0)]
if do_regression:
    # Add some interesting possibilities for backpressure patterns
    #
    # TODO : Could replace these with generator dependency injections just
    #        as with idle generation above...
    backpressure_options.extend([(0.5, 16), (5.0, 2)])

factory.add_option('sink_backpressure', backpressure_options)


#
# Stimulus image pattern generator option
#
# Each item in the list of options is a pattern generator instance, capable
# of generating a sequence of static or dynamic image patterns.
#
# TEMPORARY - Use pad bits to make images visible when clipped to RGB24
pad_bits = 0
rgb_planes = RGBPlanes.NUM_PLANES.value
vert_mono = MonoGradient(direction=PatternGenerator.GradientDir.VERT, num_planes=rgb_planes)
horiz_mono = MonoGradient(direction=PatternGenerator.GradientDir.HORIZ, num_planes=rgb_planes)
random = UniformRandom(num_planes=rgb_planes, pad_bits=pad_bits)
rgb_gradient = RgbGradient(pad_bits=pad_bits)

pattern_options = [ rgb_gradient ]
if do_regression:
    pattern_options.extend([ vert_mono, horiz_mono, random ])
    
factory.add_option('pattern_generator', pattern_options)


#
# Bayer pattern row and column modes
#
row_options = [BayerPattern.RowMode.RED_FIRST]
if do_regression:
    row_options.append(BayerPattern.RowMode.BLUE_FIRST)

factory.add_option('bayer_row_mode', row_options)

col_options = [BayerPattern.ColumnMode.GREEN_FIRST]
if do_regression:
    col_options.append(BayerPattern.ColumnMode.GREEN_SECOND)

factory.add_option('bayer_column_mode', col_options)


# TODO - Add an option for inter-frame spacing cycles
#
# This must include some varied spacing as well as a directed case for
# back-to-back frames (header-payload-header-payload-... without any gaps)
        

# Generate the set of tests to run
factory.generate_tests()
