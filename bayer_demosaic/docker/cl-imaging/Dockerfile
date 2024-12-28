FROM cocotb-1.0:latest

MAINTAINER Eldridge M. Mount IV <Eldridge_Mount@mksinst.com>

# Install additional packages for image processing
RUN sudo apt-get update \
    && sudo apt-get upgrade -y \
    && sudo apt-get install -y \
        eog

# Install Python packages for image processing
RUN sudo pip install \
    matplotlib \
    numpy \
    Pillow \
    scipy
