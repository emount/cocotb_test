#!/bin/bash
#
# File        : ip-sim.sh
# Author      : Eldridge M. Mount IV
# Description : Generic simulation environment launch utilities
# 
#      o  0
#      | /       Copyright (c) 2017-2019
#     (CL)---o   Critical Link, LLC
#       \
#        O

#
# Function to launch a Docker image for IP core simulation
#
function launch_sim_env {
    # Capture arguments
    CORE_NAME=${1}
    CONTAINER=${2}
    
    # Define the user's home directory, as mapped into the container
    CONTAINER_HOME="/home/${USER}"
    
    # Bind mounts are declared as <source_path>:<target_path>
    #
    # * Map the parent directory to a core-named subdirectory in the container
    # * Map the 'ip_common' utilities directory for use of common source files
    declare -a BIND_MOUNTS=(
        "$(pwd)/..:${CONTAINER_HOME}/${CORE_NAME}"
        "$(pwd)/../../ip_common:${CONTAINER_HOME}/ip_common"
    )
    
    # Begin with a bind mount for sharing the X11 socket connection with
    # the container, making the local window environment seamless.
    MOUNT_ARGS="--mount type=bind,source=/tmp/.X11-unix,target=/tmp/.X11-unix "
    for bindMount in "${BIND_MOUNTS[@]}"
    do
        IFS=':' read -r -a bindArray <<< "${bindMount}"
        MOUNT_ARGS="${MOUNT_ARGS}--mount type=bind,source=${bindArray[0]},target=${bindArray[1]} "
    done
    
    # Run the specified container with its bind mountings, setting the
    # display variable to match that of the host environment.
    IMAGE_NAME=${CONTAINER}-${CORE_NAME}
    docker run --name=${IMAGE_NAME} \
           --rm                     \
           -it                      \
           --env DISPLAY=${DISPLAY} \
           ${MOUNT_ARGS}${CONTAINER}
}
