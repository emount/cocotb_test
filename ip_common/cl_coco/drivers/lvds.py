"""
File        : lvds.py
Author      : Eldridge M. Mount IV
Description : Cocotb module providing generalized LVDS drivers

              The module provide LVDS stimulus of various kinds, by way
              of hierarchical modules which may be assembled into complex
              interfaces. Provisions are also made for stimulating a link
              with pseudo-randomized phase noise (jitter).


     o  0
     | /       Copyright (c) 2018-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
import cl_coco
from . import DriverError
from ..clock import Overclock, PhaseOffset

import cocotb
from cocotb.clock import Clock
from cocotb.drivers import Driver
from cocotb.log import SimLog
from cocotb.triggers import Event, RisingEdge, Timer
from cocotb.utils import get_sim_steps

from collections import deque

import logging
import numpy as np
import random


class LvdsGroup:
    """Class abstracting a group of one or more LVDS lanes"""
    
    def __init__(self,
                 entity,
                 group_name,
                 base_name,
                 clock,
                 signals,
                 word_length,
                 bus_separator="_",
                 log=None,
                 *args, **kwargs):
        """Initialize the LVDS group with its signals and parameters

        The data lanes may be provided as either positive / negative signal
        pairs, or as single-bit excitation signals for logic designed to
        operate downstream from the physical-layer LVDS receivers.

        TODO: Implement the P/N-pair use case! Only single-ended for now.

        In either case, they are provided so as to associate index zero [0]
        with the least-significant data lane, for purposes of serialization.

        For the time being, only single-data-rate (SDR) signaling is implemented.

        Args:
            entity: HDL entity to which the bus interface is bound
            group_name: "Friendly" name for the instance
            base_name: Base name prefix of the interface signals
            clock: Suffix of the clock signal to bind to
            signals: Suffixes of the bus signals to bind to
            word_length: Length of each serialized word (bits)
        """
        # Retain parameters
        self._entity = entity
        self._group_name = group_name
        self._base_name = base_name
        self.log = log
        self._word_length = word_length

        # Create an event and queue for use in sending transactions
        self._data_ready = Event(name="LvdsGroup._data_ready")
        self._data_queue = deque()
        
        # Resolve the clock signal within the entity we drive by name
        clock_name = (base_name + bus_separator + clock)
        self.clock = getattr(entity, clock_name)

        # Resolve each of the signals in the entity, adding them to a dict
        self._signals = {}
        for signal in signals:
            signal_name = (base_name + bus_separator + signal)
            entity_signal = getattr(entity, signal_name)
            self._signals[signal] = entity_signal

            # Also bind the signal to this instance, without full scoping
            #
            # This permits direct assignment to lane names, e.g. lvds_bus.D12
            setattr(self, signal, entity_signal)


    def get_num_lanes(self):
        """Returns the total number of LVDS lanes the instance drives

        The number of lanes is tallied up across instance's set of LVDS groups

        Returns:
            The total number of LVDS lanes
        """
        return len(self._signals)


    def append(self, words, event=None):
        """Appends the passed vector of words for serialization to the group"""
        self._data_queue.append((words, event))
        self._data_ready.set()


    def _reset_data(self):
        # Iterate through the data lanes, resetting them
        for name, entity_signal in self._signals.items():
            entity_signal <= 0
            

    def reset(self):
        """Resets all of the group's signals to a known state"""
        self.clock <= 0
        self._reset_data()

        
    @cocotb.coroutine
    def start(self, clock_freq, bitrate_ratio, phase_sigma=None):
        """Starts the group's clock signals and data serialization

        Once started, the group will serialize any data found on its queue.
        Zeros are driven during any cycle in which the queue is empty.

        Args:
            clock_freq: LVDS clock frequency for the group
            bitrate_ratio: Ratio of the bit rate to the LVDS clock
        
        Kwargs:
            phase_sigma: Standard deviation of initial LVDS clock phase
        """

        # Wait for half of the clock period, plus a random offset (+/-) if phase
        # sigma was specified
        phase_wait = (cl_coco.clock_steps(clock_freq) / 2.0)
        if phase_sigma is not None:
            # Select the offset from a zero-mean normal distribution, with
            # one standard deviation corresponding to 1/4 of the clock period,
            # quantized to picoseconds.
            quarter_ps = (1.0e6 / (clock_freq * 4.0))
            random_ps = int(random.gauss(0.0, (phase_sigma * quarter_ps)))
            phase_wait = (phase_wait + get_sim_steps(random_ps, 'ps'))

        # Wait for the (possibly-pseudorandomized) time
        yield Timer(phase_wait)

        # Create and start an abstraction for the overclocking scenario
        self._overclock = Overclock(self.clock, clock_freq, bitrate_ratio)
        cocotb.fork(self._overclock.start())

        # Start the data serialization thread
        cocotb.fork(self._serialize())


    @cocotb.coroutine
    def _yield_symbols(self, ideal_ticks):
        """Coroutine to yield for an integer number of symbols

        This routine is useful for converting a desired wait in simulation ticks
        into a number of LVDS driver symbols. This permits stimulus code to wait
        using convenient units of time, while doing so in a way which does not
        disrupt temporal alignment to the symbol boundaries.

        The implementation actually yields for one bit clock period less than the
        computed number of symbol-interval ticks; this allows code to queue up a
        payload of data for the instance directly after the yield returns, ensuring
        it is picked up with the identical alignment as data serialized prior to
        the call to this method.

        Args:
            ideal_ticks: The ideal number of ticks to quantize to symbol periods
        """
        # Convert to an integer number of symbol periods
        symbol_ticks = (self._overclock.period * ((self._word_length / self._overclock.ratio) - 1))
        num_symbols = int(ideal_ticks / symbol_ticks)
        
        yield Timer(num_symbols * symbol_ticks)


    @cocotb.coroutine
    def _serialize(self):
        """Coroutine to perform serialization of data for the group"""

        # Initialize the data lanes
        self._reset_data()

        # Always synchronize to the nearest clock edge once
        align = self._overclock.align
        tick = self._overclock.await_tick
        offset = PhaseOffset.QUAD_RISE
        yield align(offset)

        # Loop indefinitely for now; at some point this class should be able
        # to be stopped (and restarted) cleanly.
        while True:
            while not self._data_queue:
                # Wait for data available
                self._data_ready.clear()
                yield self._data_ready.wait()
                yield align(offset)

            # Data is avaliable, fed in matrices of data with as many rows
            # as LVDS data lanes
            (lane_data, event) = self._data_queue.popleft()
            if event is not None:
                event.clear()

            # Iterate through the data packet, slicing column vectors to serialize
            for column in lane_data.T:
                # Iterate, shifting each column of bits onto the interface at the bit rate
                bit_mask = (1 << self._word_length)
                while bit_mask > 1:
                    # Shift the next-MSB onto each lane of the group
                    bit_mask = (bit_mask >> 1)
                    signal_index = 0
                    for name, entity_signal in self._signals.items():
                        # Evaluate whether the bit is set in the word or not
                        entity_signal <= int((column[signal_index] & bit_mask) != 0)
                        signal_index = (signal_index + 1)
                
                    # Wait for the next bit interval
                    yield tick(offset)

            # The packet has been sent, fire any completion event
            if event is not None:
                event.set()

        
class LvdsDriver(Driver):
    """Class implementing a composite LVDS driver device

    An instance of this class is a fully-functional transactor for the
    task of stimulating an LVDS interface. Single- or multi-group configurations
    are supported.

    LVDS groups have their own clock, with defined intra-group specifications
    for clock-to-lane and lane-to-lane skew variance.
    """

    def __init__(self,
                 entity,
                 base_name,
                 groups,
                 clock_freq,
                 bitrate_ratio,
                 word_length,
                 *args, **kwargs):
        """Initialize and bind the LVDS driver to its bus

        The 'groups' parameter is either:

        * A one-dimensional collection of bus suffixes, beginning with the
          clock and continuing in big-endian bus ordering for data, e.g.:

          ['CLK', 'D3', 'D2, 'D1', 'D0']

        * A two-dimensional collection of parameters as above, each specifying
          a full LVDS bus group (clock + data), e.g.:

          [ ['CLK_A', 'D3_A', 'D2_A, 'D1_A', 'D0_A'],
            ['CLK_B', 'D3_B', 'D2_B, 'D1_B', 'D0_B'] ]

        In the latter case, data values are mapped to groups in big-endian
        fashion: the most-significant values are sent to the first group, etc.
        
        For now, instances unconditionally source their own internal clocks.

        Args:
            entity: HDL entity to which the bus interface is bound
            base_name: Base name prefix of the interface signals
            groups: Single or multiple set(s) of bus signal suffixes
            clock_freq: Frequency of the LVDS interface clock (MHz)
            bitrate_ratio: Ratio of the bit rate to the LVDS clock
            word_length: Length of each serialized word (bits)
        """
        # Store keyword args
        self._callback = kwargs.pop('callback', None)

        # Sanity-check input arguments as appropriate
        if self._callback is not None and not callable(self._callback):
            raise TypeError("Expected a callable compare function but got {}".format(type(self._callback)))

        # Retain parameters for generation and start the clock
        self._clock_freq = clock_freq
        self._bitrate_ratio = bitrate_ratio
        self._word_length = word_length

        # Scope the log for convenient traceability to source
        self.log = SimLog("cl_coco.drivers.{}".format(self.__class__.__name__))
        self.log.setLevel(logging.INFO)

        # Normalize the dimensionality of the groups specifier argument
        if(isinstance(groups[0], str)):
            groups = [groups]

        # TODO - Add a check for group lane width homogeneity

        # Create an abstraction for each LVDS clock / lane group
        self._groups = []
        self._events = {}
        group_index = 0
        for group in groups:
            # The first signal in each group parameter is defined as the clock,
            # followed by the data lanes, in big-endian ordering. Forward params
            # for things like different bus
            group_name = "{}[{}]".format(base_name, group_index)
            group = LvdsGroup(entity,
                              group_name,
                              base_name,
                              group[0],
                              group[1:],
                              word_length,
                              log=self.log,
                              *args,
                              **kwargs)
            self._groups.append(group)
            self._events[group] = Event("Group{}_Event".format(group_index))
            group_index = (group_index + 1)

        # Finally, initialize the base class layer
        Driver.__init__(self)


    def get_num_lanes(self):
        """Returns the total number of LVDS lanes the instance drives

        The number of lanes is tallied up across instance's set of LVDS groups

        Returns:
            The total number of LVDS lanes
        """
        num_lanes = 0
        for group in self._groups:
            num_lanes = (num_lanes + group.get_num_lanes())

        return num_lanes


    @cocotb.coroutine
    def reset(self):
        """Resets the interface signals to a known state"""
        for group in self._groups:
            yield group.reset()


    def start(self, phase_sigma=0.0):
        """Starts clocking and data serialization on all LVDS bus groups

        Clocking is performed with configurable phase relationships amongst the
        LVDS lane groups. Strictly speaking, data serialization only begins if
        data has already been enqueued.
        
        Kwargs:
            phase_sigma: Standard deviation of initial LVDS clock phase
        """
        for group in self._groups:
            cocotb.fork(group.start(self._clock_freq,
                                    self._bitrate_ratio,
                                    phase_sigma=phase_sigma))


    @cocotb.coroutine
    def _yield_symbols(self, ideal_ticks):
        """Coroutine to yield for an integer number of symbols

        This routine is useful for converting a desired wait in simulation ticks
        into a number of LVDS driver symbols. This permits stimulus code to wait
        using convenient units of time, while doing so in a way which does not
        disrupt temporal alignment to the symbol boundaries.

        Args:
            ideal_ticks: The ideal number of ticks to quantize to symbol periods
        """
        # Dispatch the actual wait to the first lane group
        yield self._groups[0]._yield_symbols(ideal_ticks)

            
    @cocotb.coroutine
    def _driver_send(self, pkt, sync=True):
        """Serializes a packet of data to the interface lanes

        The first dimension of the passed ndarray must be equal to the number
        of LVDS lanes the instance drives. Similarly, the data type of the array
        must possess at least as many bits as the instance's word length.

        Packet data is processed in network byte order (big-endian) with
        respect to the "filling in" of LVDS lanes with source packet bytes
        on each data cycle.

        Args:
            pkt: Numpy ndarray to serialize
        """
        # Sanity-check the input
        #
        num_lanes = self.get_num_lanes()
        group_lanes = self._groups[0].get_num_lanes()
        if(pkt.shape[0] != num_lanes):
            raise DriverError("First dimension of packet is {}; must be {}".format(pkt.shape[0],
                                                                                   num_lanes))

        # TODO - Check the dtype of the array and assert that its bit width
        #        accommodates the word length of the instance
        self.log.debug("Serializing {}-dimensioned packet".format(pkt.shape))

        # Iterate through the groups, slicing the overall data vector among them
        start_row = 0
        for group in self._groups:
            group_data = pkt[start_row:(start_row + group_lanes)]
            start_row = (start_row + group_lanes)
            group.append(group_data, event=self._events[group])

        # Wait for a single one of the groups to complete
        yield self._events[self._groups[0]].wait()

        # TODO - This is a bit hokey... there needs to be a better way to feed
        #        data to lane groups in parallel
        # for group in self._groups:
        #     yield self._events[group].wait()
