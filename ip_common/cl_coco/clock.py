"""
File        : clock.py
Author      : Eldridge M. Mount IV
Description : Cocotb module providing advanced clocking resources

              The module provides modules useful in more-complex clocking
              situations : overclocking, phase-lock loop modeling, etc.


     o  0
     | /       Copyright (c) 2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
import cl_coco

import cocotb
from cocotb.clock import BaseClock
from cocotb.triggers import Event, ReadOnly, Timer
from cocotb.utils import get_sim_steps

import enum
from enum import Enum


"""Enumeration for specifying phase offset"""
PhaseOffset = Enum('PhaseOffset', 'RISING QUAD_RISE FALLING QUAD_FALL', start=0)
    

class Overclock(BaseClock):
    """Class providing a 1:N clock-to-event infrastructure

    This abstraction is useful in scenarios where a clock frequency is defined,
    but other signals related to it must transition at a higher multiple.

    Clock-to-event ratios are integer-multiple only.
    """


    def __init__(self, signal, clock_freq, ratio, units=None):
        """Initialize the overclocking context
        
        Args:
            signal: HDL entity signal the clock is bound to
            clock_freq: Fundamental (clock signal) frequency (MHz)
            ratio: Overclocking ratio at which the internal event fires
        
        Kwargs:
            units: Units period is specified in, None for simulation timesteps
        """
        # Initialize the signal binding
        BaseClock.__init__(self, signal)

        # Derive and retain clock period information, initialize state
        self.ratio = ratio
        self.period = cl_coco.clock_steps(clock_freq, ratio=1)
        self._tick_period = cl_coco.clock_steps(clock_freq, ratio=(2 * ratio))

        events_low = int(ratio / 2)
        events_high = (events_low + int(ratio % 2))
        self._ticks_low = (2 * events_low)
        self._ticks_high = (2 * events_high)
        self._tick_count = (self._ticks_high - 1)
        self._tick_parity = False
        
        # Create the internal event for clients
        self._event = Event(name='overclock_event')
        self._signal_last = 0
        

    @cocotb.coroutine
    def start(self):
        """Starts the activity of internal events and external clock edges"""
        tick = Timer(self._tick_period)

        # Initialize the clock with a rising edge
        self.signal <= 1

        # Loop, processing ticks
        self._tick_parity = False
        while True:
            # Evalulate whether to fire the event this iteration; it is fired
            # first, before conditionally updating the HDL signal
            self._event.set()
            self._tick_parity = not self._tick_parity

            if(self._tick_count == 0):
                if self._signal_last:
                    self.signal <= 0
                    self._tick_count = (self._ticks_low - 1)
                else:
                    self.signal <= 1
                    self._tick_count = (self._ticks_high - 1)
            else:
                self._tick_count = (self._tick_count - 1)

            # Sample the present value of the signal for the next edge
            yield ReadOnly()
            self._signal_last = self.signal.value
            yield tick


    @cocotb.coroutine
    def align(self, offset=None):
        """Suspends execution until aligned with the offset

        Args:
            offset: Synchronizes to offset w.r.t. rising edge of the clock
        """
        # Return immediately for sync alignments if already synchronized.
        while not self.__is_synced(offset):
            self._event.clear()
            yield self._event.wait()


    @cocotb.coroutine
    def await_tick(self, offset=PhaseOffset.RISING):
        """Suspends execution until a tick has elapsed

        Args:
            quadrature: Tick in quadrature with the clock signal
        """
        if((offset is PhaseOffset.QUAD_RISE) or (offset is PhaseOffset.QUAD_FALL)):
            test_count = 1
        else:
            test_count = 0
            
        synced = False
        while not synced:
            self._event.clear()
            yield self._event.wait()
            synced = ((self._tick_count % 2) == test_count)


    def __is_synced(self, offset):
        """Returns true if the instance is temporally synchronized"""
        if((offset is PhaseOffset.RISING) or (offset is PhaseOffset.QUAD_RISE)):
            edge_case = not self._signal_last
        else:
            edge_case = self._signal_last
            
        if((offset is PhaseOffset.QUAD_RISE) or (offset is PhaseOffset.QUAD_FALL)):
            test_count = 1
        else:
            test_count = 0
            
        return ((self._tick_count == test_count) and edge_case)
