## -*- docker-image-name: "cocotb-1.0" -*-
FROM conda/miniconda3:latest

# Heritage of the file, as well as its principal user
MAINTAINER Eldridge M. Mount IV <eldridge.mount@gmail.com>
ENV GITHUB_USER emount

# Arguments for the image, as well as defaults
ARG SHELL_USER=simulator
ARG SHELL_UID=1000
ARG SHELL_GID=1000

# Set environment variables intended for use within the container
ENV EDITOR=emacs
ENV TERM=xterm

# Reconfigure the default shell as bash instead of dash
RUN rm /usr/bin/sh; ln -s /usr/bin/bash /usr/bin/sh

# Update the package cache and install packages that we will require
RUN apt-get update \
    && apt-get upgrade -y \
    && apt-get install -y \
      sudo \
      expect \
      less \
      git

# GHDL is written in ADA, which requires both GCC and the GNAT compiler
# suites and GNU make. In addition, we are building GHDL with the LLVM
# back-end for top simulation speed.
RUN apt-get install -y clang gcc g++ gnat llvm make python3-dev swig

# Install various utilities that will be useful
RUN apt-get install -y bash-completion emacs git-gui gtkwave


# Fixups for Python installation
#
# Add a symbolic link - the installed python executable has an incorrect
# implementation of distutils.sysconfig.get_python_inc()...
RUN mkdir -p /usr/local/lib/python3.6/include && \
    ln -s /usr/local/include/python3.6m /usr/local/lib/python3.6/include/python3.6m

# Locate the TCL library
ENV TCL_LIBRARY=/usr/lib/tcl8.5

# Replace 1000 with a generic user / group id
ENV HOME /home/${SHELL_USER}
RUN export uid=${SHELL_UID} gid=${SHELL_GID} && \
    mkdir -p /home/${SHELL_USER} && \
    echo "${SHELL_USER}:x:${uid}:${gid}:${SHELL_USER},,,:${HOME}:/bin/bash" >> /etc/passwd && \
    echo "${SHELL_USER}:x:${uid}:" >> /etc/group && \
    echo "${SHELL_USER} ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/${SHELL_USER} && \
    chmod 0440 /etc/sudoers.d/${SHELL_USER} && \
    chown ${uid}:${gid} -R ${HOME}

# Switch to the new user and go to their home directory
USER ${SHELL_USER}
WORKDIR ${HOME}

# Fetch, build, and install both GHDL and cocotb as the ${SHELL_USER}
#
# This image pulls my personal fork of cocotb to get bug fixes, and even
# macroscopic feature additions via merge pull requests.
#
# My repository was originally branched from the following URL:
#
# https://github.com/potentialventures/cocotb.git
#
# A similar approach is used for GHDL, with a personal fork of the official
# repository for insulating my Docker images from unexpected changes.
#
# My repository was originally branched from the following URL:
#
# https://github.com/tgingold/ghdl.git

# Clone and build GHDL from source
#
# Bust the cache on this fetch by getting the ref for the branch and
# adding it as a local file to the image. If the file content changes,
# the layer and its children are invalidated.
ENV GHDL_BRANCH master
ADD https://api.github.com/repos/${GITHUB_USER}/ghdl/git/refs/heads/${GHDL_BRANCH} ghdl_ref.json
RUN git clone --branch ${GHDL_BRANCH} https://github.com/${GITHUB_USER}/ghdl.git
RUN mkdir ghdl-build; cd ghdl-build; ../ghdl/configure --srcdir=../ghdl --prefix=/usr/local --with-llvm-config
RUN cd ghdl-build; make; sudo make install

# Clone and install cocotb
#
# Bust the cache on this fetch as well
ENV COCOTB_BRANCH ${GITHUB_USER}_master
ADD https://api.github.com/repos/${GITHUB_USER}/cocotb/git/refs/heads/${COCOTB_BRANCH} cocotb_ref.json
RUN git clone --branch ${COCOTB_BRANCH} https://github.com/${GITHUB_USER}/cocotb.git

# Apply unified-diff content from the following potentialventures/cocotb
# pull requests (see individual patch files for excruciating detail):
#
# Introducing Functional Coverage and Constrained Random Features #490
# Patch to fix problems with module-relative imports from #490
ENV COCOTB_PATCH_490=cocotb-add-constrained-randomization.patch
ADD patches/${COCOTB_PATCH_490} /tmp/${COCOTB_PATCH_490}
RUN patch --directory=./cocotb --strip=1 < /tmp/${COCOTB_PATCH_490}
ENV COCOTB_PATCH_490_FIX=cocotb-fix-crv-imports.patch
ADD patches/${COCOTB_PATCH_490_FIX} /tmp/${COCOTB_PATCH_490_FIX}
RUN patch --directory=./cocotb --strip=1 < /tmp/${COCOTB_PATCH_490_FIX}

# Provide a bashrc script for the user, as well as a "smoke test" script
# for validating the container
COPY content/user-bashrc ${HOME}/.bashrc
RUN sudo chown ${SHELL_UID}:${SHELL_GID} ${HOME}/.bashrc
RUN chmod 755 ${HOME}/.bashrc
COPY content/smoketest.sh ${HOME}/smoketest.sh
RUN sudo chown ${SHELL_UID}:${SHELL_GID} ${HOME}/smoketest.sh
RUN chmod 755 ${HOME}/smoketest.sh

# As a default behavior, go to a console as a development image.
# Derived images may override this with an ENTRYPOINT in order
# to run a CI verification node, etc.
CMD /bin/bash
