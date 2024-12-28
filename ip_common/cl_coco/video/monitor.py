"""
File        : monitor.py
Author      : Eldridge M. Mount IV
Description : Module with classes and definitions for monitoring video


     o  0
     | /       Copyright (c) 2017-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
from .cl_video_defs import VideoFormats, VideoFramePacking, VideoProcessing
from ..imaging.image_defs import RGBPlanes

import cocotb
from cocotb.crv import Randomized
from cocotb.monitors.avalon import AvalonSTPkts
from cocotb.result import TestFailure
from cocotb.triggers import ReadOnly, RisingEdge

import imageio


class VideoMonitorError(Exception):
    """Exception class for the video monitor infrastructure"""
    pass


class VideoMonitor(AvalonSTPkts):
    """Class for monitoring a CL Video stream

       This module receives data coming from a CL Video stream,
       reconstructing images for response validation / visualization.
    """

    def __init__(self,
                 entity,
                 name,
                 clock,
                 likelihood,
                 max_delay,
                 pix_per_clk,
                 plane_names,
                 **kwargs):
        """Initializes the instance, starting its activity

        Args:
            entity: Handle to the entity being monitored
            name: Name for the monitor
            clock: Avalon-ST interface clock
            likelihood: Percent likelihood of a delay to occur
            max_delay: Maximum waitready delay for the sink
            pix_per_clk: Number of pixels per clock cycle
            plane_names: Collection of image planes names, implies number
        """
        # Store derived class arguments
        self._pix_per_clk = pix_per_clk
        self._plane_names = plane_names
        self._num_planes = len(plane_names)

        # The configuration specifies physical layer details of endianness,
        # bits per logical symbol, and cycles of ready latency. The packing
        # of pels is performed in little-endian order, as is the order of
        # header elements.
        config = {
            'dataBitsPerSymbol' : VideoFramePacking.PEL_LANE_BITS,
            'firstSymbolInHighOrderBits' : False,
            'maxChannel' : 0,
            'readyLatency' : VideoFramePacking.READY_LATENCY
        }

        # Dispatch to our base class implementation to initialize the
        # fundamental, inherited monitoring behavior
        AvalonSTPkts.__init__(self,
                              entity,
                              name,
                              clock,
                              callback=self._verify_rx_header,
                              config=config,
                              **kwargs)

        # Start the backpressure randomization thread
        self.log.info("Starting video monitor")
        cocotb.fork(self.gen_waitready(likelihood, max_delay))


    def _verify_rx_header(self, pkt):
        """Callback for use in processing received Avalon-ST frames

        This method responds to the raw frame reception monitor callback
        mechanism, verifying the content of each received frame's header

        Args:
            pkt: String of byte data representing the received packet
        """
        self.log.info("Verifying header and unpacking {}-byte Avalon-ST packet".format(len(pkt)))
        
        # Begin by extracting the frame-level details from the header.
        #
        # Header data is replicated for each image plane; the first plane header
        # is used for parsing, and any other planes' headers are merely tested for
        # agreement with the first.
        if len(pkt) < VideoFramePacking.NUM_HDR_BYTES:
            raise VideoMonitorError("Packet of length ({}) bytes is too small to contain a header".format(len(pkt)))

        # Validate the replicated header data, which arrives in beats sized
        # based upon the number of pixels per clock
        beat_bytes = int(self._pix_per_clk * VideoFramePacking.PEL_LANE_BITS / 8)
        header_beats = int(VideoFramePacking.NUM_HDR_BYTES / beat_bytes)
        for beat_index in range(header_beats):
            beat_offset = (beat_index * beat_bytes * self._num_planes)
            zero_header = pkt[beat_offset:(beat_offset + beat_bytes)]
            for plane_index in range(1, self._num_planes):
                plane_offset = (beat_offset + (plane_index * beat_bytes))
                plane_header = pkt[plane_offset:(plane_offset + beat_bytes)]
                if(plane_header != zero_header):
                    err_string = "Header for \'{}\' image plane disagrees with that of \'{}\' plane"
                    raise VideoMonitorError(err_string.format(plane_names(plane_index),
                                                              plane_names(0)))
        
        # Extract the header values for use in image and format reconstruction
        [horiz_res,
         vert_res,
         roi_off_x,
         roi_off_y,
         frame_index,
         timestamp] = VideoFramePacking.unpack_header(pkt, log=self.log)
        video_format = {'horiz_res' : horiz_res,
                        'vert_res' : vert_res}

        # Compute the expected payload size given the number of image planes and
        # the extracted format's resolution
        header_bytes = (self._num_planes * VideoFramePacking.NUM_HDR_BYTES)
        payload_bytes = (horiz_res *
                         vert_res *
                         self._num_planes *
                         VideoFramePacking.PIXEL_FIELD_BYTES);
        expected_length = (header_bytes + payload_bytes)
        if(len(pkt) != expected_length):
            raise VideoMonitorError("Packet length ({}) does not match expected ({}) for video format: {}".format(len(pkt), expected_length, video_format))

        # Unpack the image frame data
        rx_image = VideoProcessing.unpack_image(video_format, self._num_planes, pkt)
        
        self.log.info("Successfully extracted video frame :")
        self.log.info("  * Video format : {}".format(video_format))

        # Save the received image to a file
        #
        # TODO - Make this instance-configurable for path, etc.
        imageio.imwrite("rx_frame{}.png".format(frame_index), rx_image)
        

    class ReadyDelay(Randomized):
        """Class for constrained-random generation of waitready signals"""
        
        def __init__(self, likelihood, max_delay):
            """Initializes an instance constrained to randomize with a max delay

            Args:
                likelihood: Percent likelihood of a delay to occur
                max_delay: Maximum number of delay cycles before ready
            """
            Randomized.__init__(self)
            self._likelihood = int(likelihood * 10)
            self._max_delay = max_delay
            self._dice = 0
            self.delay = 0
            self.addRand('_dice', range(1000))
            self.addRand('delay', range(max_delay))

            
        def post_randomize(self):
            """Post-process the raw randomized values"""
            if(self._dice >= self._likelihood):
                self.delay = 0
                

    @cocotb.coroutine
    def gen_waitready(self, likelihood, max_delay, log=None):
        """Coroutine to run in parallel, randomly generating the ready signal

        Args:
            likelihood: Percent likelihood of a delay to occur
            max_delay: Maximum number of delay cycles before ready
        """
        clk_rise = RisingEdge(self.clock)
        ready_delay = VideoMonitor.ReadyDelay(likelihood, max_delay)

        # Determine whether to generate backpressure as an ongoing activity
        if(max_delay == 0):
            # Assert ready indefinitely
            self.log.info("  * No backpressure configured")
            self.bus.ready <= 1
            yield RisingEdge(self.clock)
        else:
            # Generating a random distribution of backpressure cycles.
            # Begin with readiness; the waitready latency of the video stream requires
            # the sink to assert readiness before the input may drive for active cycles
            self.log.info("  * Random backpressure configured")
            while True:
                # Wait for signals to settle, then test whether the present cycle is an
                # active beat of data transfer
                yield ReadOnly()
                active_beat = self.bus.valid.value
                yield clk_rise
            
                if active_beat:
                    # Active beat; decide whether to delay after receiving this beat
                    ready_delay.randomize()
                    if ready_delay.delay:
                        self.bus.ready <= 0
                        for delay in range(ready_delay.delay):
                            yield clk_rise
                            
                # Assert ready
                self.bus.ready <= 1
