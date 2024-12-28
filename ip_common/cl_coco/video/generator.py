"""
File        : generator.py
Author      : Eldridge M. Mount IV
Description : Module furnishing components for generating CL Video-formatted
              (Avalon-ST) video stream stimulus.

     o  0
     | /       Copyright (c) 2017-2019
    (CL)---o   Critical Link, LLC
      \
       O
o"""
from .cl_video_defs import VideoFormats, VideoFramePacking, VideoProcessing
from ..drivers.lvds import LvdsDriver
from ..imaging.image_defs import ImageDefs

from abc import ABC, abstractmethod

import cocotb
from cocotb.drivers.avalon import AvalonSTPkts
from cocotb.result import TestFailure

import imageio

import pydoc


class VideoGenError(Exception):
    """Exception class for the video generation infrastructure"""
    pass


class CLVideoGenerator(AvalonSTPkts):
    """Class providing CL Video-formatted stimulus"""


    def __init__(self, video_format, colorspace, *args, **kwargs):
        """Initializes an instance of a CL-Video generator instance

        Args:
            video_format: Video format to generate stimulus for
            colorspace: Colorspace associated with the stimulus
        """
        # Store arguments consumed by this layer
        self._video_format = video_format
        self._colorspace = colorspace
        
        # Store keyword args
        self._pel_depth = kwargs.pop('pel_depth', None)
        self._pix_per_clk = kwargs.pop('pix_per_clk', None)
        self._callback = kwargs.pop('callback', None)

        # Sanity-check input arguments
        if self._callback and not callable(self._callback):
            raise TypeError("Expected a callable compare function but got {}".format(type(self._callback)))

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
        AvalonSTPkts.__init__(self, *args, config=config, **kwargs)
        

    @cocotb.coroutine
    def start_stimulus(self,
                       num_frames,
                       generator,
                       video_format,
                       video_params,
                       event=None,
                       transform=None,
                       log=None):
        """Starts the input stimulus for the test

        Args:
            num_frames: Number of frames to generate
            generator: Stimulus image generator instance
            video_format: Video format dict
            video_params: Video parameters dict
        
        Kwargs:
            transform: Optional transform function
        """
        yield AvalonSTPkts.start_stimulus(self)
        try:
            # Generate video frames of the selected, receiver-compatible video format,
            # using the passed source image generator.
            #
            # Use a nonzero timestamp, for sake of interest (randomize this?)
            timestamp = 64
            frame_index = 0
            for orig_frame in generator.images(num_frames,
                                               self._video_format['vert_res'],
                                               self._video_format['horiz_res'],
                                               self._colorspace['pel_bits']):
                # First save the original generator-rendered image to a PNG file for
                # debugging and visualization
                #
                # TODO - Make this instance-configurable, by name, etc.
                if transform is not None:
                    imageio.imwrite("orig_frame{}.png".format(frame_index), orig_frame)

                # Apply any passed transform to the source image
                stim_frame = orig_frame if transform is None else transform(orig_frame, log=log)

                # Write the actual stimulus frame, post-transform (if any)
                imageio.imwrite("stim_frame{}.png".format(frame_index), stim_frame)

                # Perform the actual link-layer encoding and enqueuing for physical-
                # layer transmission by our base class
                transaction = self.__encode_frame(stim_frame, frame_index, timestamp)

                # Forward the full transaction to any set callback, our base class ensures
                # any value set is a callable object
                if self._callback is not None:
                    self._callback(transaction)

                # Send the fully-constructed transaction to the DUT over the Avalon-ST physical layer
                yield self.send(transaction)
                
                self.log.info("Serialized stimulus frame {}".format(frame_index))
                frame_index = (frame_index + 1)

            # Fire any optionally-provided event as soon as all frames have been sent
            self.log.info("Stimulus exhausted")
            if event is not None:
                event.set()
                
        except VideoGenError as error:
            raise TestFailure("Video generator error :\n  * {}".format(error))


    def __encode_frame(self, frame, frame_index, timestamp):
        """Helper method for encoding generated video frames for transmission

        This method performs the link-layer encoding of source image data into
        header-wrapped and properly padded / packed frames of data for serialization
        at the physical Avalon-ST layer.

        Args:
            frame: The stimulus frame to wrap
            frame_index: Frame index to encode
            timestamp: Timestamp to encode
        """
        # TODO - Create a generator-based pattern for the client test to be
        #        able to modulate ROI offset abstractly on a frame-to-frame
        #        basis.
        #
        # For now, simply hard-code a reasonably-interesting ROI offset
        roi_off = {'x' : 12, 'y' : 99}
        packed_frame = VideoFramePacking.pack_header(self._video_format,
                                                     roi_off,
                                                     frame_index,
                                                     timestamp,
                                                     log=self.log)

        # Append the image data as big-endian packed bytes
        #
        # TODO - There has to be a vectorized numpy method for doing this rather
        #        than a stupid, slow loop...
        image_rows, image_cols, image_planes = frame.shape
        for row in range(image_rows):
            for col in range(image_cols):
                pel_value = frame[row][col]
                packed_frame = (packed_frame + chr(pel_value & 0x0FF) + chr(pel_value >> 8))

        return packed_frame


class LvdsVideo(ABC, LvdsDriver):
    """Specialization of the generic LVDS bus driver class for video applications

    This class layer provides some convenience methods useful for stimulating
    LVDS video receiver DUTs with various image generators. Generators are
    supplied via test factory dependency injection.
    """

    def __init__(self, entity, base_name, groups, video_format, colorspace, *args, **kwargs):
        """Initialization for an LVDS video driver instance

        Args:
            entity: HDL entity to which the bus interface is bound
            base_name: Base name prefix of the interface signals
            video_format: Video format to generate stimulus for
            colorspace: Colorspace associated with the stimulus
        """
        # Store arguments required by this layer
        self._video_format = video_format
        self._colorspace = colorspace

        # Initialize the physical interface base class layer
        LvdsDriver.__init__(self,
                            entity,
                            base_name,
                            groups,
                            *args,
                            **kwargs)


    @abstractmethod
    def _encode_frame(self, frame):
        """Encodes and enqueues the passed frame of video

        Subclasses implementing this method perform the interface-specific
        task of slicing, transposing, sync-prefixing, and otherwise encapsulating
        the passed frame for transmission to its intended DUT. Once this encoding
        is performed, the resulting link-layer protocol data unit is enqueued
        for physical-layer transmission by the base class.

        Args:
            frame: Image to be sent as a video frame
        """
        pass

    
    @cocotb.coroutine
    def _send_frames(self, num_frames, generator, event=None):
        """Sends the specified number of frames over the interface

        The passed video generator is used as a source of interface-agnostic
        NumPy image frame data, which may be either static or dynamic from
        frame to frame.

        The base LVDS driver layer must already have been started before
        calling this method.
        
        Args:
            num_frames: Number of stimulus frames to generate
            generator: Stimulus image generator instance

        Kwargs:
            event: Optional event to fire when completed
        """
        try:
            # Generate video frames of the selected, receiver-compatible video format,
            # using the passed source image generator. Each dependency-injected generator
            # renders single-plane data, either natively or through a Bayer transform
            # wrapper generator operating on original RGB-space imagery.
            #
            # The driver's colorspace is used independent of the passed video format's
            # native colorspace.
            #
            # Catch and re-raise any local exceptions as test failures.
            # Cocotb result classes subclass from StopIteration, which will simply
            # stop the generator iteration as if normal.
            frame_index = 0
            for frame in generator.images(num_frames,
                                          self._video_format['vert_res'],
                                          self._video_format['horiz_res'],
                                          self._colorspace['pel_bits']):
                # First save the generator-rendered image to a PNG file for debugging
                #
                # TODO - Make this instance-configurable, by name, etc.
                imageio.imwrite("stim_frame{}.png".format(frame_index), frame)
                
                # Forward the video frame to any set callback, our base class ensures
                # any value set is a callable object
                if self._callback is not None:
                    self._callback(frame)

                # Perform the actual link-layer encoding and enqueuing for physical-
                # layer transmission by our base class
                yield self._encode_frame(frame)

                self.log.info("Serialized stimulus frame {}".format(frame_index))
                frame_index = (frame_index + 1)

            # Fire any optionally-provided event as soon as all frames have been sent
            self.log.info("Stimulus exhausted")
            if event is not None:
                event.set()
                
        except VideoGenError as error:
            raise TestFailure("Video generator error :\n  * {}".format(error))
