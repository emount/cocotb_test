"""
File        : __init__.py
Author      : Eldridge M. Mount IV
Description : Module initialization file for package cl_coco

              This package provides Critical Link infrastructure for
              the Cocotb verification environment.

              Some generally-useful utilities are also declared directly
              herein.

     o  0
     | /       Copyright (c) 2018-2019
    (CL)---o   Critical Link, LLC
      \
       O
"""
__all__ = ['drivers', 'imaging', 'video']

import cocotb
from cocotb.clock import Clock
from cocotb.utils import get_sim_steps


def clock_steps(clock_freq, ratio=1):
    """Returns the number of simulation timesteps for a clock period

    Args:
        clock_freq: Clock frequency (MHz)

    Kwargs:
        ratio: Ratio to the specified clock frequency
    """
    return get_sim_steps(int(1.0e6 / (clock_freq * ratio)), 'ps')


def start_clock(clock_freq, signal):
    """Starts a free-running clock on the passed HDL signal

    Args:
        clock_freq: Clock frequency (MHz)
        signal: HDL signal to drive with the new clock signal
    """
    cocotb.fork(Clock(signal, clock_steps(clock_freq)).start())
