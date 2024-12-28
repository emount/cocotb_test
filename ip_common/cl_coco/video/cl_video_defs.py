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
from ..imaging.image_defs import Colorspace, ImageDefs

import binascii

import cocotb
from cocotb.binary import BinaryValue
from cocotb.result import TestFailure

import numpy

import struct


class VideoFormats:
    """Static class housing definitions for standard and test video formats

    All video formats are expressed using a dict with a standardized set of keys.
    Video formats do not, at present, encompass blanking intervals; only active
    picture area is considered.
    """

    def period_ns(frame_rate):
        """Converts the passed frame rate into a frame period in nsec

        Args:
            frame_rate: Frame rate, in Hz
        
        Returns:
            The corresponding frame period, in nsec
        """
        return int(1.0e9 / frame_rate)
    

    """Tiny format, useful for fast simulations of multiple frames"""
    VID_FMT_TINY = {
        'horiz_res' : 256,
        'vert_res'  : 16,
        'period_ns' : period_ns(50.0)
    }
    

    """Quarter-CIF format"""
    VID_FMT_QCIF = {
        'horiz_res' : 176,
        'vert_res'  : 120,
        'period_ns' : period_ns(50.0)
    }

    
    """VESA XGA format"""
    VID_FMT_VESA_XGA = {
        'horiz_res' : 1024,
        'vert_res'  : 768,
        'period_ns' : period_ns(50.0)
    }

    
    """Abbreviated VESA XGA format for simulation"""
    VID_FMT_VESA_XGA_ABBREV = {
        'horiz_res' : 1024,
        'vert_res'  : 8,
        'period_ns' : period_ns(50.0)
    }

    
    """4K format"""
    VID_FMT_4K_UHD = {
        'horiz_res' : 3840,
        'vert_res'  : 2160,
        'period_ns' : period_ns(50.0)
    }

    
    """Abbreviatd 4K format for simulation"""
    VID_FMT_4K_UHD_ABBREV = {
        'horiz_res' : 3840,
        'vert_res'  : 16,
        'period_ns' : period_ns(50.0)
    }
    

class VideoProcessing:
    """Class housing definitions for video processing with scipy, PIL, etc."""

    @staticmethod
    def unpack_image(video_format, num_planes, pkt):
        """Unpacks the passed packet of video data into an image

        Args:
            video_format: Video format dict
            num_planes: Number of image planes
            pkt: The video packet to unpack

        Returns:
            An ndarray containing the unpacked image
        """
        # Compute the expected payload size given the format, and compute the skip
        # index past the video packet header, which is replicated for each plane.
        header_bytes = (num_planes * VideoFramePacking.NUM_HDR_BYTES)
        payload_bytes = (video_format['horiz_res'] *
                         video_format['vert_res'] *
                         num_planes *
                         VideoFramePacking.PIXEL_FIELD_BYTES);

        # Sanity-check the input packet; strictly check for the exact number of bytes;
        # there is presently no use case for unpacking frames from larger buffers, so
        # we can be restrictive.
        total_bytes = (header_bytes + payload_bytes)
        if(len(pkt) != total_bytes):
            raise TestFailure("Packet size ({}) does not strictly match frame bytes ({})".format(total_bytes,
                                                                                                 len(pkt)))

        # Allocate an empty ndarray of the proper shape to fill
        array_shape = VideoProcessing.ndarray_shape(video_format, num_planes)
        image_data = numpy.empty(array_shape, dtype='uint16')
        payload_index = header_bytes
        for vert_index in range(video_format['vert_res']):
            for horiz_index in range(video_format['horiz_res']):
                for plane_index in range(num_planes):
                    # Unpack each pixel in little-endian byte order
                    image_data[vert_index, horiz_index, plane_index] = ((ord(pkt[payload_index + 1]) << 8) |
                                                                        ord(pkt[payload_index]))
                    payload_index += 2
                      
        return image_data


    @staticmethod
    def ndarray_shape(video_format, num_planes):
        """Returns a tuple of (height, width, planes) for the passed format / params
    
        The returned tuple is directly applicable to 'numpy.ndarray' sizes for use
        with numpy image processing operations (via scipy / PIL, etc.)
    
        Args:
            video_format: Video format dict
            num_planes: Number of image planes

        Returns:
            A tuple of (height, width, planes) suitable for numpy.ndarray sizing
        """
        # Delegate to the generalized image processing implementation
        return ImageDefs.ndarray_shape(video_format['vert_res'],
                                       video_format['horiz_res'],
                                       num_planes)

    
class VideoFramePacking:
    """Class housing definitions for framing video within headers"""

    """Standardized field width for packing pixel elements

    All pixels are packed, MSB-justified, into this width of a field within
    video streams. This ends up being a free notational convenience, as no
    logic is generated for unused padding bits during synthesis.
    """
    PEL_LANE_BITS = 16

    """Ready latency is one, as with all Altera video IP cores"""
    READY_LATENCY = 1
    
    """Prototypical video parameters dict

    A video parameters dict informs video generators of the parameters within
    which they are constrained to generate and pack video data.
    """
    VID_PARAMS_RGB24 = {
        'colorspace' : Colorspace.RGB24,
    }
       
    VID_PARAMS_RGB27 = {
        'colorspace' : Colorspace.RGB27,
    }
       
       
    VID_PARAMS_RGB30 = {
        'colorspace' : Colorspace.RGB30,
    }
       
       
    VID_PARAMS_RGB33 = {
        'colorspace' : Colorspace.RGB33,
    }
       
    VID_PARAMS_RGB36 = {
        'colorspace' : Colorspace.RGB36,
    }
        
       
    """Constant header quadlet (32-bit) offsets and field definitions"""
    HDR_QUADLET_FRAME_SIZE = 0
    HDR_QUADLET_ROI_OFFSET = 1
    HDR_QUADLET_FRAME_INDEX = 2
    HDR_QUADLET_TIMESTAMP = 3
    HDR_QUADLET_RSVD_0 = 4
    HDR_QUADLET_RSVD_1 = 5
    HDR_QUADLET_RSVD_2 = 6
    HDR_QUADLET_RSVD_3 = 7
    NUM_HDR_QUADLETS = 8

    BYTES_PER_QUADLET = 4

    NUM_HDR_BYTES = (NUM_HDR_QUADLETS * BYTES_PER_QUADLET)

    
    """Number of bits and bytes each pixel value is packed into"""
    PIXEL_FIELD_BYTES = 2
    PIXEL_FIELD_BITS = (PIXEL_FIELD_BYTES * 2)
    

    @staticmethod
    def pack_header(video_format, roi_off, frame_index, timestamp, log=None):
        """Packs a header for a Critical Link video frame

        Args:
            format - Dict containing format definitions
            roi_off - Dict specifying the (x, y) offset for the ROI
            timestamp - 32-bit timestamp for the frame, in nsec

        Returns:
            A string of bytes representing the video header
        """
        # Frame width / height quadlet
        hdr_values = [video_format['horiz_res'],
                      video_format['vert_res']]

        # Begin the format with a little-endian directive; the overall swap
        # of bytes at the end converts to big-endian as a group.
        hdr_format = '<HH'

        # ROI offset quadlet
        hdr_values.extend([roi_off['x'],
                           roi_off['y']])
        hdr_format += 'HH'

        # Frame index quadlet
        hdr_values.append(frame_index)
        hdr_format += 'L'

        # Frame timestamp quadlet
        hdr_values.append(timestamp)
        hdr_format += 'L'

        # Drive some identifiable values for the reserved words
        hdr_values.extend([0xDEADBEEF, 0xD0C0FFEE, 0xABCDEF01, 0xFABD00D])
        hdr_format += 'LLLL'
        
        # Pack the formatted fields into a string of bytes
        packer = struct.Struct(hdr_format)
        packed_bytes = packer.pack(*hdr_values)

        # Convert to a string for return
        # TODO: There *has* to be a better way to do this.
        packed_str = ""
        for byte_val in packed_bytes:
            packed_str = (packed_str + chr(byte_val))

        return packed_str
        

    @staticmethod
    def unpack_header(frame_data, log=None):
        """Unpacks a header from a Critical Link video frame

        Args:
            frame_data - Byte string containing video frame data

        Returns:
            A tuple of extracted values
        """
        # Frame width / height quadlet
        horiz_res = None
        vert_res = None
        hdr_format = '<HH'
        hdr_values = [horiz_res, vert_res]
        hdr_bytes = 4

        # ROI offset quadlet
        roi_off_x = None
        roi_off_y = None
        hdr_format += 'HH'
        hdr_values.extend([roi_off_x, roi_off_y])
        hdr_bytes += 4

        # Frame index quadlet
        frame_index = None
        hdr_format += 'L'
        hdr_values.append(frame_index)
        hdr_bytes += 4
         
        # Frame timestamp quadlet
        timestamp = None
        hdr_format += 'L'
        hdr_values.append(timestamp)
        hdr_bytes += 4
        
        # Unpack the header with the accumulated format string
        header_bytes = bytearray()
        for byte_index in range(hdr_bytes):
            header_bytes.append(ord(frame_data[byte_index]))
            
        return struct.unpack(hdr_format, header_bytes)
