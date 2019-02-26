#!/bin/sh
#
# Runs `act c fuzz` several times on a single C litmus test, outputting
# the results into a directory.

if [ "$#" -ne "2" ];
then
    echo "Usage: $0 INFILE COUNT"
    exit
fi

# Parameters
infile="${1}"
count="${2}"

# Programs
ACT=${ACT="act"}

dir_name="fuzz_$(date '+%y_%m_%d_%H_%M_%S')"

mkdir "${dir_name}"

for k in $(seq "${count}");
do
    "${ACT}" c fuzz "${infile}" -o "${dir_name}/${k}.c.litmus"
done

# If we look like we're on a Mac, open the directory in Finder.
if [ "$(uname)" = "Darwin" ];
then
    open "${dir_name}"
fi
