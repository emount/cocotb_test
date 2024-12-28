#!/bin/bash
#
# Simulation environment setup for the bayer_demosaic IP core
USER=simulator
USER_HOME="/home/${USER}"

# Bind mounts are declared as <source_path>:<target_path>
declare -a BIND_MOUNTS=(
    "$(pwd):${USER_HOME}/bayer_demosaic" 
)

# Begin with a bind mount for sharing the X11 socket connection with
# the container, making the local window environment seamless.
MOUNT_ARGS="--mount type=bind,source=/tmp/.X11-unix,target=/tmp/.X11-unix "
for bindMount in "${BIND_MOUNTS[@]}"
do
    IFS=':' read -r -a bindArray <<< "${bindMount}"
    MOUNT_ARGS="${MOUNT_ARGS}--mount type=bind,source=${bindArray[0]},target=${bindArray[1]} "
done

# Run the cl-imaging image with its bind mountings, setting the
# display variable to match that of the host environment.
SIM_IMAGE=cli-quartus-17.1
docker run --name=${USER}-${SIM_IMAGE} --rm -it --env DISPLAY=${DISPLAY} ${MOUNT_ARGS}${SIM_IMAGE}
