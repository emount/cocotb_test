#!/bin/bash
###
# File        : gen_sim_wrapper.sh
# Author      : Eldridge M. Mount IV
# Description : Shell script to render a specific configuration of the
#               top-level simulation wrapper. Generics are substituted
#               into the top level via constants.
#
#     o  0
#     | /       Copyright (c) 2017-2018
#    (CL)---o   Critical Link, LLC
#      \
#       O

# Capture input arguments
if [[ $# != 6 ]]; then
    echo "Usage : $0 <pel_depth> <pix_per_clk> <fpga_family> <interp_kernel_idx> <supports_backpressure> <max_horiz_res>"
    exit -1
fi

pel_depth=$1
pix_per_clk=$2
fpga_family=$3
interp_kernel_idx=$4
supports_backpressure=$5
max_horiz_res=$6

echo "Generating top-level wrapper :"
echo "  * Bits / pel            : ${pel_depth}"
echo "  * Pixels / beat         : ${pix_per_clk}"
echo "  * FPGA family           : ${fpga_family}"
echo "  * Interpolation kernel  : ${interp_kernel_idx}"
echo "  * Supports backpressure : ${supports_backpressure}"
echo "  * Max line resolution   : ${max_horiz_res}"

# RGB processing entails three color planes
num_rgb_planes=3

# Pixels are always packed into a fixed-length lane
pel_lane_bits=16

#
# Compute derived values required for substitution
#

# High bit of the input / output stream data vectors
input_data_bits=$((pix_per_clk * pel_lane_bits))
input_data_high=$((input_data_bits - 1))
output_data_bits=$((input_data_bits * num_rgb_planes))
output_data_high=$((output_data_bits - 1))

# Width of the "empty" signal; this is an optional signal, but the
# cocotb framework insists on it, and it's not a big enough deal that
# I want to diverge on my fork of cocotb just for this.
#
# Both input and output empty bits are specified separately, and are
# computed with a fixed symbol size of eight bits. Since pixels are packed
# unconditionally as 16-bit fields, there is a factor of two atop of the
# raw number of pixels per clock.
function log2 {
    local x=0
    for (( y=$1-1 ; $y > 0; y >>= 1 )) ; do
        let x=$x+1
    done
    echo $x
}
empty_vec_in_width=$((${pix_per_clk} * 2))
empty_vec_in_high=$((${empty_vec_in_width} - 1))
empty_vec_out_width=$((${empty_vec_in_width} * ${num_rgb_planes}))
empty_vec_out_high=$((${empty_vec_out_width} - 1))

#
# Perform substitutions on the template file to produce the necessary
# instantiation for the test(s)
#
sed -e "s/<FPGA_FAMILY>/\"${fpga_family}\"/" \
    -e "s/<PEL_DEPTH>/${pel_depth}/" \
    -e "s/<PIX_PER_CLK>/${pix_per_clk}/" \
    -e "s/<SUPPORTS_BACKPRESSURE>/${supports_backpressure}/" \
    -e "s/<INPUT_DATA_HIGH>/${input_data_high}/" \
    -e "s/<OUTPUT_DATA_HIGH>/${output_data_high}/" \
    -e "s/<EMPTY_VEC_IN_HIGH>/${empty_vec_in_high}/" \
    -e "s/<EMPTY_VEC_OUT_HIGH>/${empty_vec_out_high}/" \
    -e "s/<INTERP_KERNEL_IDX>/${interp_kernel_idx}/" \
    -e "s/<MAX_HORIZ_RES>/${max_horiz_res}/" \
        < ./templates/sim_wrapper_template.vhd > ./sim_wrapper.vhd
