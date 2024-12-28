"""
File        : scoreboard.py
Author      : Eldridge M. Mount IV
Description : Module implementing a scoreboard for verification of expected
              image data for a video pipeline implementation.

     o  0
     | /       Copyright (c) 2018-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
from .cl_video_defs import VideoProcessing

import cocotb
from cocotb import logging
from cocotb.result import TestFailure
from cocotb.scoreboard import Scoreboard


class VideoScoreboard(Scoreboard):
    """Class implementing a video-specific scoreboard

    Basic scoreboard semantics are inherited from the parent class.
    The comparison method is overridden to only focus on video payload.
    Headers are skipped in the received transactions, and expected images
    are used for comparison and resolution of error locations to pels.
    """

    def __init__(self, dut, video_format, colorspace, pix_per_clk, plane_names=None):
        """Initializes the instance and its underlying base class

        The instance is parameterized with the video format and parameter
        metadata associated with the test. This provides the comparison
        implementation with the information needed to extract images from
        received transactions and bound operations.

        Args:
            dut: Device under test the scoreboard is associated with
            video_format: Video format dict
            colorspace: Colorspace being used with the format

        Kwargs:
            plane_names: Sequence of string names for image planes
        """
        Scoreboard.__init__(self, dut)
        self._video_format = video_format
        self._colorspace = colorspace
        self._plane_names = plane_names
        self._pix_per_clk = pix_per_clk
        # self.log.setLevel(logging.DEBUG)
        

    def compare(self, got, exp, log, strict_type=True):
        """Overloaded comparison function specific to video frame data
        
        Each received packet is compared for video payload data against
        the corresponding expected input. No reordering of packets is
        permitted.

        Args:
            got: Received transaction to check
            exp: Expected frame data
        """
        # Extract the RGB image from the transaction
        dut_image = VideoProcessing.unpack_image(self._video_format,
                                                 self._colorspace['num_planes'],
                                                 got)

        if dut_image.shape != exp.shape:
            self.log.error("Extracted image of shape {}, expected {}".format(dut_image.shape, exp.shape))
            raise TestFailure("Unable to reconstruct proper frame from DUT transaction")
    
        self.log.debug("Extracted correctly-shaped image {} from DUT output".format(dut_image.shape))

        # Loop through the images, comparing them for identical content
        for row_index in range(exp.shape[0]):
            for col_index in range(exp.shape[1]):
                for plane_index in range(exp.shape[2]):
                    dut_pel = dut_image[row_index, col_index, plane_index]
                    exp_pel = exp[row_index, col_index, plane_index]
                    if(dut_pel != exp_pel):
                        plane_label = self._plane_names[plane_index] if self._plane_names else plane_index
                        pixel_beat = int(((row_index * self._video_format['horiz_res']) + col_index) / self._pix_per_clk)
                        self.log.error("Incorrect pel value at ({}, {}) plane ({}) : received 0x{:04X}, expected 0x{:04X}, pixel beat {}".format(col_index,
                                                                                                                                                 row_index,
                                                                                                                                                 plane_label,
                                                                                                                                                 dut_pel,
                                                                                                                                                 exp_pel,
                                                                                                                                                 pixel_beat))

                        # TODO - Identify frame by both sequence and its encoded number
                        raise TestFailure("Mismatched image data on frame (???)")
