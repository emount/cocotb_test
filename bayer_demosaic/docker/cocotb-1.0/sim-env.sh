#!/bin/bash
#
# Starting point for using the cocotb image, with an example
# set of directory bindings

USER=simulator
USER_HOME="/home/${USER}"

# Bind mounts are declared as <source_path>:<target_path>
declare -a BIND_MOUNTS=(
    "$(pwd)/foo:${USER_HOME}/foo" 
    "$(pwd)/bar:${USER_HOME}/bar"
)

# Begin with a bind mount for sharing the X11 socket connection with
# the container, making the local window environment seamless.
MOUNT_ARGS="--mount type=bind,source=/tmp/.X11-unix,target=/tmp/.X11-unix "
for bindMount in "${BIND_MOUNTS[@]}"
do
    IFS=':' read -r -a bindArray <<< "${bindMount}"
    MOUNT_ARGS="${MOUNT_ARGS}--mount type=bind,source=${bindArray[0]},target=${bindArray[1]} "
done

# Run the cocotb-1.0 image with its bind mountings, setting the
# display variable to match that of the host environment.
docker run --name=cocotb --rm -it --env DISPLAY=${DISPLAY} ${MOUNT_ARGS}cocotb-1.0
