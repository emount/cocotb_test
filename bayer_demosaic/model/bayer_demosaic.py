"""
File        : bayer_demosaic.py
Author      : Eldridge M. Mount IV
Description : Module containing a psuedo-driver for the bayer_demosaic
              core, as well as behavioral models, etc. comprising an
              intelligent, automated testbench.

     o  0
     | /       Copyright (c) 2017-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
import cl_coco
from cl_coco.imaging.image_defs import RGBPlanes
from cl_coco.imaging.transform import BayerPattern
from cl_coco.video.cl_video_defs import VideoFormats, VideoFramePacking, VideoProcessing
from cl_coco.video.generator import CLVideoGenerator
from cl_coco.video.monitor import VideoMonitor
from cl_coco.video.scoreboard import VideoScoreboard

import cocotb
from cocotb.binary import BinaryValue
from cocotb.drivers.avalon import AvalonMaster
from cocotb.result import TestFailure
from cocotb.triggers import NextTimeStep, ReadOnly, RisingEdge, Timer
from cocotb.utils import get_sim_steps

import numpy


class BayerDemosaic:
    """Class abstracting an instance of the bayer_demosaic IP core"""

    #
    # Register file constant definitions
    #

    # Control register
    __CTRL_REG_ADDRESS = 0x00
    __CORE_DISABLE = 0x00000000
    __CORE_ENABLE = 0x80000000
    __BAYER_COL_GREEN_FIRST = 0x00000000
    __BAYER_COL_GREEN_SECOND = 0x00000004
    __BAYER_ROW_RED_FIRST = 0x00000000
    __BAYER_ROW_BLUE_FIRST = 0x00000002
    __CAPTURE_HEADER = 0x00000001
      
    # Interrupt flags and mask registers
    __IRQ_FLAGS_REG_ADDRESS = 0x04
    __IRQ_MASK_REG_ADDRESS = 0x05
    __IRQ_CAPTURE = 0x00000001
    
    # Video header capture registers
    __RES_CAPTURE_ADDRESS = 0x08
    __ROI_CAPTURE_ADDRESS = 0x09
    __INDEX_CAPTURE_ADDRESS = 0x0A
    __TS_CAPTURE_ADDRESS = 0x0B
    __WHITE_BALANCE_RED_ADDRESS = 0x0C
    __WHITE_BALANCE_GREEN_ADDRESS = 0x0D
    __WHITE_BALANCE_BLUE_ADDRESS = 0x0E

    # Fractional representation constants for white balance gain
    __WHITE_BALANCE_MANT_BITS = 1
    __WHITE_BALANCE_FRACT_BITS = 15
    __WHITE_BALANCE_BITS = (__WHITE_BALANCE_MANT_BITS +
                            __WHITE_BALANCE_FRACT_BITS)


    #
    # Common IP core register ROM address / assorted declarations
    #
  
    # Core ID register
    #
    # ID word is the lower 32-bit slice of sha256('bayer_demosaic')
    __ID_REG_ADDRESS = 0x80
    __ID_WORD = 0x3AEFEE02
    
    # Core revision register
    __REVISION_REG_ADDRESS = 0x81
    __REVISION_MAJOR = 1
    __REVISION_MINOR = 0
    __REVISION_FIELD_BITS = 8
    __REVISION_FIELD_MASK = 0x0FF
    
    # Core capabilities register
    __CAPS_REG_ADDRESS = 0x82
    __BP_SUPPORT_FLAG   = 0x80000000
    __PEL_DEPTH_MASK    = 0x0000000F
    __PIX_PER_CLK_SHIFT = 4
    __PIX_PER_CLK_MASK  = 0x0000000F
      

    def __init__(self, dut, log):
        """Initializes the instance

        Args:
            dut (cocotb.handle.HierarchyObject): The dut handle
            log (cocotb.log.SimBaseLog): The cocotb log context to use
        """
        self._dut = dut
        self._log = log
        self.same_clocks = dut.Same_Clocks
        self.host_clock = dut.Host_Clock
        self.host_reset = dut.Host_Reset
        self.input_clock = dut.Input_Clock
        self.input_reset = dut.Input_Reset
        self.output_clock = dut.Output_Clock
        self.output_clock_muxed = dut.output_Clock_Muxed
        self.output_reset = dut.Output_Reset


    @cocotb.coroutine
    def init_test(self,
                  same_clocks,
                  input_clock_freq,
                  output_clock_freq,
                  host_clock_freq,
                  reset_usec,
                  sink_backpressure,
                  pattern_generator,
                  row_mode,
                  column_mode,
                  video_format,
                  video_params,
                  num_frames,
                  idle_inserter=None,
                  log=None):
        """Initializes a test using the instance

        Args:
            same_clocks: Flag indicating whether input_clock == output_clock
            input_clock_freq: Input pixel pipeline clock frequency, in MHz
            output_clock_freq: Output pixel pipeline clock frequency, in MHz
            host_clock_freq: Host clock frequency, in MHz
            reset_usec: Reset duration, in usec
            sink_backpressure: Tuple of (likelihood %, max_delay) for backpressure
            pattern_generator: Pattern generator instance to use for stimulus images
            row_mode: Bayer subsampling row mode
            column_mode: Bayer subsampling column mode
            video_format: Video format for the test
            video_params: Video parameters for the test format
            num_frames: Number of frames to run for the test
            idle_inserter: Generator of idle cycles for the input stimulus
        """
        self._log.info("Initializing test")

        # Save parameters for later use
        self._row_mode = row_mode
        self._column_mode = column_mode
        self._video_format = video_format
        self._video_params = video_params

        # TODO - Bring white balance gains out as a parameter for variation
        self._white_balance_gains = (1.0, 1.0, 1.0)

        # Fork free-running clocks for the pipeline and host interfaces
        self._input_clock_freq = input_clock_freq
        self._output_clock_freq = output_clock_freq
        self._host_clock_freq = host_clock_freq
        cl_coco.start_clock(input_clock_freq, self.input_clock)
        cl_coco.start_clock(host_clock_freq, self.host_clock)
        
        # Conditionally start the output clock based upon whether the input
        # clock is being multiplexed to the output clock anyway
        self._same_clocks_flag = same_clocks
        self.same_clocks <= 1 if same_clocks else 0
        if not same_clocks:
            # The output clock is distinct, start its oscillation
            cl_coco.start_clock(output_clock_freq, self.output_clock)
        else:
            # The output clock is the input, don't generate another
            self.output_clock <= 0
        
        # Create the Avalon-MM master driver
        self._host_master = AvalonMaster(self._dut, "Host", self.host_clock)

        # Reset the instance, placing it into a known state for the test
        yield self._reset(get_sim_steps(reset_usec, 'us'))

        # Check for the module ID and revision
        yield self._validate_instance()

        # Create the Avalon-ST source for stimulating the input, now that the
        # interface parameters are known from the instance capabilities. A
        # callback is supplied for transforming input stimulus frames into
        # expected scoreboard output transactions
        self.input_source = CLVideoGenerator(self._video_format,
                                             self._video_params['colorspace'],
                                             self._dut,
                                             'Input',
                                             self.input_clock,
                                             pel_depth=self._pel_depth,
                                             pix_per_clk=self._pix_per_clk,
                                             callback=self._feed_scoreboard)
        if idle_inserter:
            self.input_source.set_valid_generator(idle_inserter())

        # Similarly, create the Avalon-ST sink which monitors the DUT output
        # stream. The monitor extracts video frames from the stream for forwarding
        # to other testbench components (scoreboards, etc.)
        #
        # Initialize the output monitor with the configured ready deassertion
        # likelihood and maximum delay configuration, only if the core supports
        # backpressure
        null_backpressure = (0.0, 0)
        if(self._bp_supported):
            # Permit whatever backpressure options are configured by the test
            gated_backpressure = sink_backpressure
        else:
            # Don't permit any backpressure, the DUT as configured cannot handle it
            if(sink_backpressure != null_backpressure):
                self._log.warning("Unable to configure test backpressure settings, DUT does not support")
            
            gated_backpressure = null_backpressure
            

        # Instantiate a component to monitor the video stream and extract images,
        # if link layer is valid
        rgb_names = []
        for plane_index in range(RGBPlanes.NUM_PLANES.value):
            rgb_names.append(RGBPlanes(plane_index).name)
        self.output_sink = VideoMonitor(self._dut,
                                        'Output',
                                        self.output_clock_muxed,
                                        gated_backpressure[0],
                                        gated_backpressure[1],
                                        self._pix_per_clk,
                                        rgb_names,
                                        reset=self.output_reset)

        # Create a scoreboard and add the monitor to it as an interface. The
        # list of images is fed as expected frames for the scoreboard
        self._scoreboard_images = []
        self.scoreboard = VideoScoreboard(self._dut,
                                          self._video_format,
                                          self._video_params['colorspace'],
                                          self._pix_per_clk,
                                          plane_names=rgb_names)
        self.scoreboard.add_interface(self.output_sink,
                                      self._scoreboard_images,
                                      strict_type=True)

        # Fork a thread for the ISR method
        cocotb.fork(self._bayer_isr())

        # Enable the module into the requested mode
        yield self._enable_core(row_mode, column_mode)

        # Arm a capture of the video header fields
        yield self._arm_header_capture()

        # Pause briefly afer arming the capture
        yield Timer(get_sim_steps(1.0, 'us'))

        # Start the input stimulus, configuring the input video generator and
        # specifying the Bayer sub-sampling method with a specifically-chosen
        # row and column mode as the transform.
        transform = lambda source_image, log=None: BayerPattern.bayer_subsample(source_image,
                                                                                row_mode,
                                                                                column_mode,
                                                                                log=log)
        yield self.input_source.start_stimulus(num_frames,
                                               pattern_generator,
                                               video_format,
                                               video_params,
                                               transform=transform,
                                               log=log)
        

    @cocotb.coroutine
    def _reset(self, steps):
        """Performs a hardware reset of the instance

        Args:
            steps: Simulation steps to hold the instance in reset for
        """
        # Set the initial condition of input signals prior to initialization of drivers
        self._dut.input_valid <= 0
        self._dut.input_startofpacket <= 0
        self._dut.input_endofpacket <= 0
        
        # Initially place the pixel pipeline in reset
        self.input_reset <= 1
        self.output_reset <= 1
        
        # Reset the host interface
        host_rising = RisingEdge(self.host_clock)
        self.host_reset <= 1
        yield Timer(steps)
        yield host_rising
        self.host_reset <= 0
        yield host_rising

        # Synchronize the pixel resets to their respective clocks
        yield RisingEdge(self.input_clock)
        self.input_reset <= 0
        yield RisingEdge(self.output_clock_muxed)
        self.output_reset <= 0


    @cocotb.coroutine
    def _validate_instance(self):
        """Validates the instance hardware against expectations"""
        self._log.info("Validating device via register file...")

        try:
            # Perform validation atomically
            self._host_master._acquire_lock()
            
            # Check the ID register and confirm the expected value is there
            id = yield self._host_master.read(BayerDemosaic.__ID_REG_ADDRESS)
            if(id != BayerDemosaic.__ID_WORD):
                raise TestFailure("Failed to match core ID register; read 0x{:08X}, expected 0x{:08X}".format(id.integer, BayerDemosaic.__ID_WORD))
            else:
                self._log.info("  * Matched core ID value 0x{:08X}".format(id.integer))
    
            revision_word = yield self._host_master.read(BayerDemosaic.__REVISION_REG_ADDRESS)
            major = (revision_word.integer >> BayerDemosaic.__REVISION_FIELD_BITS)
            minor = (revision_word.integer & BayerDemosaic.__REVISION_FIELD_MASK)
            if((major != BayerDemosaic.__REVISION_MAJOR) or (minor != BayerDemosaic.__REVISION_MINOR)):
                raise TestFailure("Core revision mismatch: found ({}.{}), expected ({}.{})".format(major, minor, BayerDemosaic.__REVISION_MAJOR, BayerDemosaic.__REVISION_MINOR))
            else:
                self._log.info("  * Found expected core revision ({}.{})".format(major, minor))
    
            # Read the capabilities word, extracting each of its fields
            capabilities = yield self._host_master.read(BayerDemosaic.__CAPS_REG_ADDRESS)
            capsWord = capabilities.integer

            self._bp_supported = ((capsWord & BayerDemosaic.__BP_SUPPORT_FLAG) != 0)
            self._log.info("  * Backpressure     : {}upported".format("S" if self._bp_supported else "Not s"))
            
            self._pel_depth = ((capsWord & BayerDemosaic.__PEL_DEPTH_MASK) + 1)
            self._log.info("  * Pel depth        : {} bits".format(self._pel_depth))
    
            exponent = ((capsWord >> BayerDemosaic.__PIX_PER_CLK_SHIFT) &
                        BayerDemosaic.__PIX_PER_CLK_MASK);
            self._pix_per_clk = 1
            for shiftCount in range(exponent):
                self._pix_per_clk = (self._pix_per_clk << 1)
            self._log.info("  * Pixels per clock : {}".format(self._pix_per_clk))
        finally:
            self._host_master._release_lock()
    
        # The bus master returns after entering a read-only sync, so advance
        # to the next simulation time step
        yield NextTimeStep()
        

    @cocotb.coroutine
    def _enable_core(self, row_mode, column_mode):
        """Enables the core in the requested mode

        Args:
            row_mode: Bayer pattern row mode
            column_mode: Bayer pattern column mode
        """
        try:
            # Perform configuration atomically
            self._host_master._acquire_lock()

            # Construct the contents of the control register and write them
            if(column_mode == BayerPattern.ColumnMode.GREEN_FIRST):
                column_bits = BayerDemosaic.__BAYER_COL_GREEN_FIRST
            else:
                column_bits = BayerDemosaic.__BAYER_COL_GREEN_SECOND
            
            if(row_mode == BayerPattern.RowMode.RED_FIRST):
                row_bits = BayerDemosaic.__BAYER_ROW_RED_FIRST
            else:
                row_bits = BayerDemosaic.__BAYER_ROW_BLUE_FIRST

            # Configure the white balance gain registers
            self._log.info("Configuring white balance gains : {}".format(self._white_balance_gains))
            gain = self._white_balance_gains[RGBPlanes.RED.value]
            yield self._host_master.write(BayerDemosaic.__WHITE_BALANCE_RED_ADDRESS,
                                          BayerDemosaic.__to_white_balance_gain(gain))
            gain = self._white_balance_gains[RGBPlanes.GREEN.value]
            yield self._host_master.write(BayerDemosaic.__WHITE_BALANCE_GREEN_ADDRESS,
                                          BayerDemosaic.__to_white_balance_gain(gain))
            gain = self._white_balance_gains[RGBPlanes.BLUE.value]
            yield self._host_master.write(BayerDemosaic.__WHITE_BALANCE_BLUE_ADDRESS,
                                          BayerDemosaic.__to_white_balance_gain(gain))
            
            # Assert the core enable bit as an effect of this call
            self._log.info("Enabling Bayer demosaic core")
            ctrl_reg = (BayerDemosaic.__CORE_ENABLE | column_bits | row_bits)

            yield self._host_master.write(BayerDemosaic.__CTRL_REG_ADDRESS, ctrl_reg)
        finally:
            self._host_master._release_lock()
        

    @cocotb.coroutine
    def _arm_header_capture(self):
        """Performs a capture of the header fields"""
        try:
            # Perform validation atomically
            self._host_master._acquire_lock()
            
            # First, clear and arm the capture interrupt
            yield self._host_master.write(BayerDemosaic.__IRQ_FLAGS_REG_ADDRESS, BayerDemosaic.__IRQ_CAPTURE);
            irq_mask = yield self._host_master.read(BayerDemosaic.__IRQ_MASK_REG_ADDRESS)
            irq_mask = (irq_mask.integer & BayerDemosaic.__IRQ_CAPTURE)
            yield self._host_master.write(BayerDemosaic.__IRQ_MASK_REG_ADDRESS, irq_mask);
            
            # Arm the capture, retaining static control register field values
            ctrl_reg = yield self._host_master.read(BayerDemosaic.__CTRL_REG_ADDRESS)
            ctrl_reg = (ctrl_reg.integer | BayerDemosaic.__CAPTURE_HEADER)
            yield self._host_master.write(BayerDemosaic.__CTRL_REG_ADDRESS, ctrl_reg)
        finally:
            self._host_master._release_lock()


    @cocotb.coroutine
    def _bayer_isr(self):
        """Interrupt service routine for the model"""
        # Loop, waiting for rising interrupt edges
        host_irq = self._dut.host_interrupt
        while True:
            # Wait for a high level on the interrupt line
            yield ReadOnly()
            while host_irq.value.integer == 0:
                yield RisingEdge(host_irq)
                yield ReadOnly()
            yield NextTimeStep()
            
            try:
                # Perform validation atomically
                self._host_master._acquire_lock()

                # Read and mask the interrupt flags, then clear them
                irq_flags = yield self._host_master.read(BayerDemosaic.__IRQ_FLAGS_REG_ADDRESS)
                irq_mask = yield self._host_master.read(BayerDemosaic.__IRQ_MASK_REG_ADDRESS)
                irq_flags = (irq_flags.integer | irq_mask.integer)
                yield self._host_master.write(BayerDemosaic.__IRQ_FLAGS_REG_ADDRESS, irq_flags)

                # Check to see if this is a capture completing
                if(irq_flags & BayerDemosaic.__IRQ_CAPTURE):
                    self._log.info("[IRQ] Header capture completed, validating")

                    # Validate the captured video resolution
                    res_word = yield self._host_master.read(BayerDemosaic.__RES_CAPTURE_ADDRESS)
                    horiz_res = (res_word.integer & 0xFFFF)
                    vert_res = ((res_word.integer >> 16) & 0xFFFF)
                    horiz_expect = self._video_format['horiz_res']
                    vert_expect = self._video_format['vert_res']
                    if((horiz_res != horiz_expect) or (vert_res != vert_expect)):
                        raise TestFailure("  * Captured resolution ({}x{}) mismatches expected ({}x{})".format(horiz_res,
                                                                                                               vert_res,
                                                                                                               horiz_expect,
                                                                                                               vert_expect))

                    # Validate the captured region of interest
                    roi_word = yield self._host_master.read(BayerDemosaic.__ROI_CAPTURE_ADDRESS)
                    horiz_roi = (roi_word.integer & 0xFFFF)
                    vert_roi = ((roi_word.integer >> 16) & 0xFFFF)

                    # TODO - Get this value coordinated between the video_generator and the test
                    horiz_expect = 12
                    vert_expect = 99
                    if((horiz_roi != horiz_expect) or (vert_roi != vert_expect)):
                        raise TestFailure("  * Captured ROI offset ({}x{}) mismatches expected ({}x{})".format(horiz_roi,
                                                                                                               vert_roi,
                                                                                                               horiz_expect,
                                                                                                               vert_expect))

                    # Validate the frame index, which should be the first
                    frame_index = yield self._host_master.read(BayerDemosaic.__INDEX_CAPTURE_ADDRESS)
                    index_expect = 0
                    if(frame_index.integer != index_expect):
                        raise TestFailure("  * Captured frame index ({}) mismatches expected ({})".format(frame_index.integer, index_expect))

                    # Validate the frame timestamp
                    frame_timestamp = yield self._host_master.read(BayerDemosaic.__TS_CAPTURE_ADDRESS)

                    # TODO - Also get this value coordinated
                    timestamp_expect = 64
                    if(frame_timestamp.integer != timestamp_expect):
                        raise TestFailure("  * Captured frame timestamp (0x{:08X}) mismatches expected (0x{:08X})".format(frame_timestamp.integer, timestamp_expect))

                    # Having made it to here, all checks have passed
                    self._log.info("  * Header capture validated")
            finally:
                self._host_master._release_lock()

        
    def _feed_scoreboard(self, transaction):
        """Callback for input stimulus transactions

        This method performs a behavioral transformation of the input,
        Bayer-patterned input frames into the demosaiced output frames
        the DUT is expected to produce in response.

        The full input transaction, including its headers, is used; this
        ensures that behaviorally-transformed data is precisely the same
        as that received by the DUT.
        
        Args:
            transaction: An Avalon-ST stimulus frame (including header)
        """
        # Extract the raw, single-plane image from the transaction
        image_data = VideoProcessing.unpack_image(self._video_format, 1, transaction)

        # Expand the Bayer-patterned input into an RGB frame behaviorally and
        # place it onto the list of expected images for the scoreboard. Test
        # parameters needed to perform the correct interpolation are passed in
        # through a lambda wrapper.
        self._log.debug("Bayer-processing {}-byte input image of shape {} behaviorally".format((image_data.size * 2), image_data.shape))
        demosaic_image = numpy.fromfunction(lambda row, col, plane: BayerDemosaic.__demosaic_func(row, col, plane, self._video_format, self._row_mode, self._column_mode, image_data, log=self._log),
                                            VideoProcessing.ndarray_shape(self._video_format,
                                                                          self._video_params['colorspace']['num_planes']),
                                            dtype='uint16')

        self._log.debug("Produced {}-byte demosaiced output image of shape {}".format((demosaic_image.size * 2), demosaic_image.shape))

        # Place the demosaiced imaged onto the expected list for the scoreboard
        self._scoreboard_images.append(demosaic_image)


    @staticmethod
    def __to_white_balance_gain(gain):
        """Convenience function for generating white balance gain values

        With only a single mantissa bit, white balance values are intended to
        be within the inclusive range [0.0, 1.0]. Values exceeding unity are
        not intended for use, despite being representible. There is no clamping
        implemented for post-gain pel values, creating the possibility of
        numerical overflow and wrapping.
        
        Args:
            gain: Real-valued gain to transform into a native value

        Returns:
            A fractional-fixed-point representation of the gain value

        """
        if((gain < 0.0) or (gain > 1.0)):
            raise TestFailure("Gain value ({}) is out of range [0.0, 1.0]".format(gain))
            
        return int(gain * (2 ** BayerDemosaic.__WHITE_BALANCE_FRACT_BITS))
        
                
    @staticmethod
    def __demosaic_func(row, col, plane, video_format, row_mode, column_mode, source_image, log=None):
        """Vectorized generation function for demosaicing

        Each pixel location is produced in a generative fashion, using the
        coordinates within the three-dimensional vector space of the output
        image.

        The implementation expands the single-plane, Bayer-patterned input
        image into the output image vector space using the following heuristics:

        * "Boundary" pixels in the first two rows / columns are nearest-neighbor
          interpolated
        * "Core" pixels are produced via a first-order interpolation:
          - Each input pixel element ('pel') is assigned through to its respective
            plane in the output image
          - Pixels at red and blue locations in the source image have their
            two missing planes' pel values bilinearly interpolated from four
            neighboring pels of each respective output plane
          - Pixels at green locations in the source image have their missing
            red and blue values linearly interpolated from two neighboring
            pels of each respective output plane

        Args:
            row: An image ndarray containing the Y coordinate in each pel
            col: An image ndarray containing the X coordinate in each pel
            plane: An image ndarray containing the plane index in each pel
            video_format: Video format for the test
            row_mode: Bayer pattern row mode
            column_mode: Bayer pattern column mode
            source_image: Bayer-patterned source image

        Kwargs:
            log: Optional log to use for debugging

        Returns:
            A color gradient image constructed from the source slices
        """
        # Begin by computing nearest-neighbor pixels for the boundary
        demosaic_image = numpy.empty(row.shape).astype(numpy.uint16)

        # Define the row and column indices for nearest-neighbor pel regions
        horiz_res = video_format['horiz_res']
        vert_res = video_format['vert_res']
        nn_cols = (0, 1, (horiz_res - 2), (horiz_res - 1))
        nn_rows = (0, 1, (vert_res - 2), (vert_res - 1))
            
        # TEMPORARY - Define some dummy data as a background to fill in
        demosaic_image[..., RGBPlanes.RED.value] = 0xAAAA
        demosaic_image[..., RGBPlanes.GREEN.value] = 0xBBBB
        demosaic_image[..., RGBPlanes.BLUE.value] = 0xCCCC
        
        # Process full boundary rows first for speed
        green_plane = RGBPlanes.GREEN.value
        green_origin_col = 0 if (column_mode == BayerPattern.ColumnMode.GREEN_FIRST) else 1
        red_plane = RGBPlanes.RED.value
        red_start_row = 0 if (row_mode == BayerPattern.RowMode.RED_FIRST) else 1
        blue_plane = RGBPlanes.BLUE.value
        blue_start_row = ((red_start_row + 1) % 2)
        for row_index in nn_rows:
            # Green plane pels use the horizontally-adjacent neighbor for replication
            green_next_col = ((green_origin_col + 1) % 2)
            green_nn_pels = source_image[row_index, green_origin_col::2, 0]
            demosaic_image[row_index, 0::2, green_plane] = green_nn_pels
            demosaic_image[row_index, 1::2, green_plane] = green_nn_pels

            # Red and blue pels use the nearest neighbor from either the same or
            # the next image row
            row_offset = (2 * int(row_index / 2))
            red_start_col = green_next_col if (row_mode == BayerPattern.RowMode.RED_FIRST) else green_origin_col
            blue_start_col = ((red_start_col + 1) % 2)
            even_row = ((row_index % 2) == 0)
            red_row = (row_offset + red_start_row)
            red_col = red_start_col if even_row else blue_start_col
            red_nn_pels = source_image[red_row, red_col::2, 0]
            demosaic_image[row_index, 0::2, red_plane] = red_nn_pels
            demosaic_image[row_index, 1::2, red_plane] = red_nn_pels

            blue_row = (row_offset + blue_start_row)
            blue_col = blue_start_col if even_row else red_start_col
            blue_nn_pels = source_image[blue_row, blue_col::2, 0]
            demosaic_image[row_index, 0::2, blue_plane] = blue_nn_pels
            demosaic_image[row_index, 1::2, blue_plane] = blue_nn_pels

            # Toggle the green origin column as row parity flips on each iteration
            green_origin_col = green_next_col
            
        # Process core rows next; these have NN pixels only at the left and right
        core_rows = range(2, (vert_res - 2))
        core_left = 2
        core_right = (horiz_res - 2)
        green_origin_col = 0 if (column_mode == BayerPattern.ColumnMode.GREEN_FIRST) else 1
        for row_index in core_rows:
            # Green plane pels use the horizontally-adjacent neighbor for replication
            green_next_col = ((green_origin_col + 1) % 2)
            green_nn_pel = source_image[row_index, green_origin_col, 0]
            demosaic_image[row_index, green_next_col, green_plane] = green_nn_pel

            green_right = (green_origin_col + core_right)
            green_nn_pel = source_image[row_index, green_right, 0]
            demosaic_image[row_index, (green_next_col + core_right), green_plane] = green_nn_pel

            # Red and blue pels use the nearest neighbor from either the same or
            # the next image row
            row_offset = (2 * int(row_index / 2))
            red_start_col = green_next_col if (row_mode == BayerPattern.RowMode.RED_FIRST) else green_origin_col
            blue_start_col = ((red_start_col + 1) % 2)
            even_row = ((row_index % 2) == 0)
            red_row = (row_offset + red_start_row)
            blue_row = (row_offset + blue_start_row)

            # Compute the left-side pels first
            red_col = red_start_col if even_row else blue_start_col
            red_nn_pel = source_image[red_row, red_col, 0]
            demosaic_image[row_index, 0, red_plane] = red_nn_pel
            demosaic_image[row_index, 1, red_plane] = red_nn_pel
            
            blue_col = blue_start_col if even_row else red_start_col
            blue_nn_pel = source_image[blue_row, blue_col, 0]
            demosaic_image[row_index, 0, blue_plane] = blue_nn_pel
            demosaic_image[row_index, 1, blue_plane] = blue_nn_pel

            # Offset by the core pixel width to index the right-side pels
            red_right = (red_col + core_right)
            red_nn_pel = source_image[red_row, red_right, 0]
            demosaic_image[row_index, core_right, red_plane] = red_nn_pel
            demosaic_image[row_index, (core_right + 1), red_plane] = red_nn_pel
            
            blue_right = (blue_col + core_right)
            blue_nn_pel = source_image[blue_row, blue_right, 0]
            demosaic_image[row_index, core_right, blue_plane] = blue_nn_pel
            demosaic_image[row_index, (core_right + 1), blue_plane] = blue_nn_pel
            

            # Assign-through source pixels to their corresponding locations in their
            # respective output planes, and compute the core, linearly-interpolated pels
            demosaic_image[row_index, green_origin_col::2, green_plane] = source_image[row_index, green_origin_col::2, 0]
            if row_index == red_row:
                # Assign-through the red source pels on this row
                demosaic_image[row_index, red_col::2, red_plane] = source_image[row_index, red_col::2, 0]

                # Compute bilinearly-interpolated blue values at red source locations
                core_left_start = (core_left + green_next_col)
                demosaic_image[row_index, core_left_start:core_right:2, blue_plane] = numpy.uint16(
                    (numpy.uint32(source_image[(row_index - 1), (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[(row_index - 1), (core_left_start + 1):(core_right + 1):2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), (core_left_start + 1):(core_right + 1):2, 0])) / 4)

                # Compute linearly-interpolated red and blue values at green source locations
                core_left_start = (core_left + green_origin_col)
                demosaic_image[row_index, core_left_start:core_right:2, red_plane] = numpy.uint16(
                    (numpy.uint32(source_image[row_index, (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[row_index, (core_left_start + 1):(core_right + 1):2, 0])) / 2)
                demosaic_image[row_index, core_left_start:core_right:2, blue_plane] = numpy.uint16(
                    (numpy.uint32(source_image[(row_index - 1), core_left_start:core_right:2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), core_left_start:core_right:2, 0])) / 2)
            else:
                # Assign-through the red source pels on this row
                demosaic_image[row_index, blue_col::2, blue_plane] = source_image[row_index, blue_col::2, 0]

                # Compute bilinearly-interpolated red values at blue source locations
                core_left_start = (core_left + green_next_col)
                demosaic_image[row_index, core_left_start:core_right:2, red_plane] = numpy.uint16(
                    (numpy.uint32(source_image[(row_index - 1), (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[(row_index - 1), (core_left_start + 1):(core_right + 1):2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), (core_left_start + 1):(core_right + 1):2, 0])) / 4)

                # Compute linearly-interpolated red and blue values at green source locations
                core_left_start = (core_left + green_origin_col)
                demosaic_image[row_index, core_left_start:core_right:2, blue_plane] = numpy.uint16(
                    (numpy.uint32(source_image[row_index, (core_left_start - 1):(core_right - 1):2, 0]) +
                     numpy.uint32(source_image[row_index, (core_left_start + 1):(core_right + 1):2, 0])) / 2)
                demosaic_image[row_index, core_left_start:core_right:2, red_plane] = numpy.uint16(
                    (numpy.uint32(source_image[(row_index - 1), core_left_start:core_right:2, 0]) +
                     numpy.uint32(source_image[(row_index + 1), core_left_start:core_right:2, 0])) / 2)
                                                                                
            # Compute bilinearly-interpolated green values at red / blue source locations
            core_left_start = (core_left + green_next_col)
            demosaic_image[row_index, core_left_start:core_right:2, green_plane] = numpy.uint16(
                (numpy.uint32(source_image[row_index, (core_left_start - 1):(core_right - 1):2, 0]) +
                 numpy.uint32(source_image[row_index, (core_left_start + 1):(core_right + 1):2, 0]) +
                 numpy.uint32(source_image[(row_index - 1), core_left_start:core_right:2, 0]) +
                 numpy.uint32(source_image[(row_index + 1), core_left_start:core_right:2, 0])) / 4)


            # Toggle the green origin column as row parity flips on each iteration
            green_origin_col = green_next_col
            
        return demosaic_image
