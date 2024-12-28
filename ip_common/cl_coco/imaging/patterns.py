"""
File        : patterns.py
Author      : Eldridge M. Mount IV
Description : Module for programmatically generating patterned images

              The abstract and concrete classes in this module are design to
              support a dependency-injection design pattern; instances may be
              placed into test factory collections, causing fully-autonomous
              yet unique pattern generators to be passed to the test for use.

     o  0
     | /       Copyright (c) 2017-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
from .image_defs import ImageDefs, RGBPlanes

from abc import ABC, abstractmethod

import cocotb
from cocotb.log import SimLog

import enum
from enum import Enum

import numpy as np


class PatternError(Exception):
    """Exception class for image pattern generators"""
    pass


class PatternGenerator(ABC):
    """Abstract class providing various image pattern generators

    Concrete subclasses provide specific implementations for a generator method
    returning a sequence of iterated pattern images. Pattern generators are able
    to produce either static or dynamic sequences of images, since the generator
    method is able to retain its own state space.
    """

    def __init__(self, pad_bits=0, dtype='uint16'):
        """Initializes an instance of the abstract pattern generator class
         
        Kwargs:
            dtype: Underlying data type for representing image pels
        """
        # Retain the input arguments for subclasses to make use of
        self._pad_bits = pad_bits
        self._dtype = dtype
        self.log = SimLog("cl_coco.imaging.{}".format(self.__class__.__name__))
        self.log.setLevel(cocotb.logging.INFO)


    @abstractmethod
    def images(self, num_images, vert_res, horiz_res, pel_depth):
        """Abstract generator method for producing image pattern sequences

        Iterating the value returned by this generator yields the specified
        number of images in a subclass-specific manner.

        Args:
            num_images: Number of images in the sequence
            vert_res: Vertical resolution
            horiz_res: Horizontal resolution
            pel_depth: Pixel element depth (bits) (NOTE - Presently ignored!)
        """
        pass

    
    """Enumeration for gradient directionality"""
    GradientDir = Enum('GradientDir', 'HORIZ VERT', start=0)
    

class RgbGradient(PatternGenerator):
    """Concrete pattern generator class producing colorful RGB gradient images"""

    def __init__(self, *args, **kwargs):
        """Initializes an RGB gradient image generator"""
        # Simply initialize our superclass, no parameters are locally used
        PatternGenerator.__init__(self, *args, **kwargs)

        
    def images(self, num_images, vert_res, horiz_res, pel_depth):
        """Overloaded image generator method"""
        # Iterate through the requested number of images, generating RGB gradients
        for image_index in range(num_images):
            # Generate the raw pel values using a vectorized generator function, binding
            # all the arguments it requires
            rgb_gradient = np.fromfunction(lambda row, col, plane: self.__gradient_func(row,
                                                                                        col,
                                                                                        plane,
                                                                                        pel_depth),
                                           ImageDefs.ndarray_shape(vert_res, horiz_res, RGBPlanes.NUM_PLANES.value),
                                           dtype=self._dtype)
            
            yield rgb_gradient
            
    
    def __gradient_func(self, row, col, plane, pel_depth):
        """Vectorized generation function for an RGB-space color gradient image

        The gradient image data is constructed as so for each channel:

          * Red   : Pixel value equals the X coordinate
          * Green : Pixel value equals the Y coordinate
          * Blue  : Pixel value equals [MAX_PEL - abs(x  - y)]

        Args:
            row: An image ndarray containing the Y coordinate in each pel
            col: An image ndarray containing the X coordinate in each pel
            plane: An image ndarray containing the plane index in each pel
            pel_depth: Pixel element depth (bits)

        Returns:
            A color gradient image constructed from the source slices
        """
        # Compute the individual slices in vectorized fashion, wrapping coordinate
        # values at the appropriate modulus given the pel depth
        modulus = (1 << pel_depth)
        col_slice = np.left_shift((col[..., 0] % modulus), self._pad_bits)
        row_slice = np.left_shift((row[..., 1] % modulus), self._pad_bits)
        MAX_PEL = np.left_shift(((2 ** (row_slice.dtype.itemsize << 3)) - 1), self._pad_bits)
        abs_diff = np.abs(col_slice.astype(int) - row_slice.astype(int))
        delta_slice = ((MAX_PEL - abs_diff) % modulus).astype(col_slice.dtype)

        # Construct the full gradient image from its constituent slices and return it
        gradient = np.empty(row.shape).astype(np.uint16)
        gradient[..., RGBPlanes.RED.value] = col_slice
        gradient[..., RGBPlanes.GREEN.value] = row_slice
        gradient[..., RGBPlanes.BLUE.value] = delta_slice
        
        return gradient


class MonoGradient(PatternGenerator):
    """Concrete pattern generator class producing monochromatic gradient images"""

    def __init__(self, direction, num_planes=1, *args, **kwargs):
        """Initializes a monochromatic image generator

        A sequence of static, monochromatic gradient images are generated in the
        passed direction. Identical spatial data is encoded on each image plane.

        Args:
            direction: Direction of the gradient to be generated
            num_planes: Number of image planes in the generated images
        """
        # Initialize our superclass and retain our layer of parameters
        PatternGenerator.__init__(self, *args, **kwargs)

        self._direction = direction
        self._num_planes = num_planes

    
    def images(self, num_images, vert_res, horiz_res, pel_depth):
        """Overloaded image generator method"""
        # Iterate through the requested number of images, generating monochromatic gradients
        for image_index in range(num_images):
            # Generate the raw pel values using a vectorized generator function, binding
            # all the arguments it requires
            mono_gradient = np.fromfunction(lambda row, col, plane: self.__gradient_func(row,
                                                                                         col,
                                                                                         plane,
                                                                                         pel_depth),
                                            ImageDefs.ndarray_shape(vert_res, horiz_res, self._num_planes),
                                            dtype=self._dtype)
            
            yield mono_gradient
            
            
    def __gradient_func(self, row, col, plane, pel_depth):
        """Vectorized generation function for a monochromatic gradient image
        
        For a horizontal gradient, each pixel value equals its X coordinate.
        Conversely, vertical gradient pixels each equal their Y coordinate.

        Args:
            row: An image ndarray containing the Y coordinate in each pel
            col: An image ndarray containing the X coordinate in each pel
            plane: An image ndarray containing the plane index in each pel
            pel_depth: Pixel element depth (bits)

        Returns:
            A monochromatic gradient image constructed from the source slices
        """
        # Produce either a horizontal or vertical gradient, wrapping coordinate
        # values at the appropriate modulus given the pel depth
        gradient = np.empty(row.shape).astype(np.uint16)
        modulus = (1 << pel_depth)
        if self._direction is PatternGenerator.GradientDir.HORIZ:
            # Compute the individual slices in vectorized fashion
            col_slice = np.left_shift((col[..., 0] % modulus), self._pad_bits)

            # Assign uniform data to each image plane
            for plane_index in range(self._num_planes):
                gradient[..., plane_index] = col_slice
                
        elif self._direction is PatternGenerator.GradientDir.VERT:
            # Similar to the horizontal pattern code
            row_slice = np.left_shift((row[..., 0] % modulus), self._pad_bits)
            
            for plane_index in range(self._num_planes):
                gradient[..., plane_index] = row_slice
                
        else:
            raise PatternError("Unrecognized gradient direction parameter \"{}\"".format(self._direction))
        
        return gradient

    
class UniformRandom(PatternGenerator):
    """Concrete pattern generator class producing uniformly-distributed random images"""

    def __init__(self, num_planes=1, *args, **kwargs):
        """Initializes a uniform-random image generator

        A sequence of dynamic random images consisting of uniformly-distributed,
        pseudo-random image data are generated. Different spatial data is encoded
        on each image plane.

        Args:
            num_planes: Number of image planes in the generated images
        """
        # Initialize our superclass and retain our layer of parameters
        PatternGenerator.__init__(self, *args, **kwargs)

        self._num_planes = num_planes

    
    def images(self, num_images, vert_res, horiz_res, pel_depth):
        """Overloaded image generator method"""
        # Iterate through the requested number of images, generating random data
        max_pel = ((1 << pel_depth) - 1)

        for image_index in range(num_images):
            # Begin with an empty array of the correct shape
            random_image = np.empty(ImageDefs.ndarray_shape(vert_res, horiz_res, self._num_planes),
                                    dtype=self._dtype)
            
            # Iterate across each image plane, populating each with a field of
            # uniformly-distributed random data
            for plane_index in range(self._num_planes):
                uniform_slice = np.random.uniform(0, max_pel, (vert_res, horiz_res)).astype(self._dtype)
                random_image[..., plane_index] = np.left_shift(uniform_slice, self._pad_bits)
            
            yield random_image
