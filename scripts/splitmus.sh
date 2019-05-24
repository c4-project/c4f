#!/bin/sh
#
# A simulator that works by running Litmus on a stubbed-out version of
# a litmus test in cross-compile mode, then uses act's stub generator
# to generate a more faithful test.

SCRIPTDIR=${SCRIPTDIR="./scripts"}

# Programs
ACT=${ACT="act"}
GAWK=${GAWK="gawk"}  # Used for awk scripts with GNU extensions.
AWK=${AWK=${GAWK}}  # Used for everything else.

# Parameters
infile="${1}"

# Prints the self-reported name of an assembly Litmus test to stdout.
# $1: path of Litmus file
get_litmus_name ()
{
    # The first line of a Litmus test is 'ARCH NAME', where ' ' is just
    # a regular space, so we can get the name of the test from that.
    #
    # TODO(@MattWindsor91): make act able to do this sort of litmus-test
    #                       header reading.
    infile="${1}"
    head -n1 "${infile}" | cut -d ' ' -f 2
}

strip_litmus()
{
    infile="${1}"
    blank="${2}"

    ${AWK} -f "${SCRIPTDIR}"/strip_litmus.awk -v blank="${blank}" "${infile}"
}

name=$(get_litmus_name "${infile}")

# TODO(@MattWindsor91): this'll need generalising for other
# architectures.
strip_litmus "${infile}" "mfence"



