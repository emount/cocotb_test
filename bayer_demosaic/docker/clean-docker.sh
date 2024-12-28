#!/bin/sh
#
# Simple little script to clean up after Docker operations.
# In particular, once done working with a particular image /
# family of related images, it's useful to clean up all the
# intermediate image layers created, leaving only the terminal
# images that you actually will launch into containers.

echo "Cleaning up after Docker!"

echo -e "  -- Removing exited containers --\n"
docker ps --all --quiet --filter="status=exited" | xargs --no-run-if-empty docker rm --volumes

echo -e "\n\n  -- Removing untagged images --\n"
docker rmi --force $(docker images | awk '/^<none>/ { print $3 }')

echo -e "\n\n  -- Removing volume directories --\n"
docker volume rm $(docker volume ls --quiet --filter="dangling=true")

echo -e "\n\nDone :)"
