#!/bin/bash
#
# File        : sim-env.sh
# Author      : Eldridge M. Mount IV
# Description : Simulation environment container launch script for use
#               simulating the Bayer Demosaic core.
# 
#      o  0
#      | /       Copyright (c) 2019
#     (CL)---o   Critical Link, LLC
#       \
#        O

# Source the common utilities script
source ../../ip_common/docker/ip-sim.sh

# Specify the core name and container to be used
#
# Use the Critical Link Imaging container with Quartus 17.1 GHDL libraries
CORE_NAME=bayer_demosaic
CONTAINER=cli-quartus-17.1

# Launch the simulation environment
launch_sim_env ${CORE_NAME} ${CONTAINER}
