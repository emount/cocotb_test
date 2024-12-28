"""
File        : cl_video_defs.py
Author      : Eldridge M. Mount IV
Description : Module housing definitions and utilities for working with video
              streams in logic conformant to the Critical Link standard for
              video frame encapsulation.

              Refer to document :

              "Critical Link, LLC Frame Data Packing Design
               (For Various Camera Systems)"

              Revision DRAFT employed in the present incarnation of this module.

     o  0
     | /       Copyright (c) 2017-2018
    (CL)---o   Critical Link, LLC
      \
       O
"""
import enum
from enum import Enum

    
"""Enumeration of RGB colorspace planes"""
RGBPlanes = Enum('RGBPlanes', 'RED GREEN BLUE NUM_PLANES', start=0)
    

class Colorspace:
    """Static class housing common colorspace dicts and utilities"""
    
    """Color spaces for Bayer-##"""
    BAYER24 = {
        'num_planes' : 1,
        'pel_bits' : 8
    }
    
    BAYER27 = {
        'num_planes' : 1,
        'pel_bits' : 9
    }
    
    BAYER30 = {
        'num_planes' : 1,
        'pel_bits' : 10
    }
    
    BAYER33 = {
        'num_planes' : 1,
        'pel_bits' : 11
    }
    
    BAYER36 = {
        'num_planes' : 1,
        'pel_bits' : 12
    }
    
    
    """Color spaces for RGB-##"""
    RGB24 = {
        'num_planes' : RGBPlanes.NUM_PLANES.value,
        'pel_bits' : 8
    }
    
    RGB27 = {
        'num_planes' : RGBPlanes.NUM_PLANES.value,
        'pel_bits' : 9
    }
    
    RGB30 = {
        'num_planes' : RGBPlanes.NUM_PLANES.value,
        'pel_bits' : 10
    }
    
    RGB33 = {
        'num_planes' : RGBPlanes.NUM_PLANES.value,
        'pel_bits' : 11
    }
    
    RGB36 = {
        'num_planes' : RGBPlanes.NUM_PLANES.value,
        'pel_bits' : 12
    }

    
class ImageDefs:
    """Static class housing definitions for image processing"""

    @staticmethod
    def ndarray_shape(vert_res, horiz_res, num_planes):
        """Returns a tuple of (height, width, planes) for the passed parameters
    
        The returned tuple is directly applicable to 'numpy.ndarray' sizes for use
        with numpy image processing operations (via scipy / PIL, etc.)
    
        Args:
            vert_res: Vertical resolution
            horiz_res: Horizontal resolution
            num_planes: Number of image planes

        Returns:
            A tuple of (height, width, planes) suitable for numpy.ndarray sizing
        """
        return (vert_res, horiz_res, num_planes)
