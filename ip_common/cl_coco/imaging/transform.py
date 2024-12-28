"""
File        : transform.py
Author      : Eldridge M. Mount IV
Description : Module providing various classes and definitions for image
              processing within Cocotb

     o  0
     | /       Copyright (c) 2018-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
from .image_defs import RGBPlanes
from .patterns import PatternGenerator

import enum
from enum import Enum

import numpy


class TransformError(Exception):
    """Exception class for image transforms"""
    pass


class BayerPattern:
    """Static class for Bayer-patterned image definitions and subsampling"""


    """Enumeration of Bayer pattern row-modes"""
    RowMode = Enum('RowMode', 'RED_FIRST BLUE_FIRST', start=0)


    """Enumeration of Bayer pattern column-modes"""
    ColumnMode = Enum('ColumnMode', 'GREEN_FIRST GREEN_SECOND', start=0)

    
    @staticmethod
    def bayer_subsample(source_image, row_mode, column_mode, log=None):
        """Sub-samples the source image in a Bayer pattern

        Args:
            source_image: Source image data, as a numpy.ndarray
            row_mode: RowMode value to be employed in the pattern
            column_mode: ColumnMode value to be employed in the pattern

        Returns:
            The single-plane, Bayer-subsampled image
        """
        # Sanity-check the source image for the appropriate number of planes
        source_planes = source_image.shape[2]
        if(source_planes != RGBPlanes.NUM_PLANES.value):
            raise TransformError("Incorrect number of source planes ({}) for Bayer subsampling".format(source_planes))
            
        if log:
            log.info("Subsampling image in [{} / {}] parity".format(row_mode, column_mode))

        # Initialize a monochromatic image of the same resolution and data type
        bayer_shape = (source_image.shape[0], source_image.shape[1], 1)
        bayer_image = numpy.empty(bayer_shape, source_image.dtype)

        # Subsample green data
        green_origin_col = 0 if (column_mode == BayerPattern.ColumnMode.GREEN_FIRST) else 1
        green_next_col = ((green_origin_col + 1) % 2)
        bayer_image[::2, green_origin_col::2, 0] = source_image[::2, green_origin_col::2, RGBPlanes.GREEN.value]
        bayer_image[1::2, green_next_col::2, 0] = source_image[1::2, green_next_col::2, RGBPlanes.GREEN.value]

        # Next, subsample red and blue
        red_start_row = 0 if (row_mode == BayerPattern.RowMode.RED_FIRST) else 1
        red_start_col = green_next_col if (row_mode == BayerPattern.RowMode.RED_FIRST) else green_origin_col
        bayer_image[red_start_row::2, red_start_col::2, 0] = source_image[red_start_row::2, red_start_col::2, RGBPlanes.RED.value]

        blue_start_row = ((red_start_row + 1) % 2)
        blue_start_col = ((red_start_col + 1) % 2)
        bayer_image[blue_start_row::2, blue_start_col::2, 0] = source_image[blue_start_row::2, blue_start_col::2, RGBPlanes.BLUE.value]

        if log:
            log.info("  * Bayer-subsampled image resolution : ({}, {})".format(bayer_image.shape[1],
                                                                               bayer_image.shape[0]))

        return bayer_image
    

class BayerTransform(PatternGenerator):
    """Concrete pattern generator transform class for Bayer subsampling

    This class operates as a transform, encapsulating another generator producing
    images in an RGB colorspace. This transform layer permits development of an
    ecosystem of pure-RGB pattern generator classes, with a simple transformability
    to single-plane Bayer-patterned sensor applications.

    Use of this class permits both "color-oriented" image sources as well as direct
    single-plane image generators to coexist in the same collection of generators
    for test dependency injection.
    """

    def __init__(self, source, row_mode, column_mode, *args, **kwargs):
        """Initializes a Bayer-subsampling transform generator instance

        Args:
            source: Source image generator to transform images from
            row_mode: Bayer subsampling row mode
            column_mode: Bayer subsampling column mode
        """
        # Initialize our superclass and retain our layer of parameters
        PatternGenerator.__init__(self, *args, **kwargs)
    
        self._source = source
        self._row_mode = row_mode
        self._column_mode = column_mode


    def images(self, num_images, vert_res, horiz_res, pel_depth):
        """Overloaded image generator method"""
        # Create a transform for subsampling
        transform = lambda source_image, log=None: BayerPattern.bayer_subsample(source_image,
                                                                                self._row_mode,
                                                                                self._column_mode,
                                                                                log=self.log)

        # Obtain a series of source images to transform into Bayer space
        frame_index = 0
        for frame in self._source.images(num_images, vert_res, horiz_res, pel_depth):
            # Apply the image transform and yield the Bayer-subsampled image to be
            # passed to the LVDS driver layer
            yield transform(frame, log=self.log)

            # Bump the frame index
            frame_index = (frame_index + 1)
