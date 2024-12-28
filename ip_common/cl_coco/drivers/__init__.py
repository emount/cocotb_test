"""
File        : __init__.py
Author      : Eldridge M. Mount IV
Description : Module initialization file for subpackage cl_coco.drivers

              This subpackage provides various Cocotb interface drivers,
	      organized into modules by family.

     o  0
     | /       Copyright (c) 2018-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
__all__ = ['lvds']


"""Useful constants for expressing common bitrate ratios"""
BITRATE_RATIO_SDR = 1
BITRATE_RATIO_DDR = 2


class DriverError(Exception):
    """Exception class for driver infrastructure"""
    pass
