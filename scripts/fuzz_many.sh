#!/bin/sh
#
# Runs `act c fuzz` several times on a single C litmus test, outputting
# the results into a directory.

# If we look like we're on a Mac, open directory $1 in Finder.
#
# $1: fuzzer directory
open_directory ()
{
    dir_name="${1}"

    if [ "$(uname)" = "Darwin" ];
    then
        open "${dir_name}"
    fi
}

# Runs $1's mutation $4 times on $3, putting the results in $2.
# $1: act command
# $2: fuzzer output directory
# $3: name of file to mutate
# $4: count
fuzz_file ()
{
    act="${1}"
    dir_name="${2}"
    in_file="${3}"
    count="${4}"

    base_name="$(basename "${in_file}" .litmus | tr '.' '_')"

    for k in $(seq "${count}");
    do
        out_file="${dir_name}/${base_name}_${k}.litmus"
        "${act}" c fuzz "${in_file}" -o "${out_file}"
    done
}

usage ()
{
    echo "Usage: $0 COUNT INFILES..."
    exit
}

if [ "$#" -lt "2" ];
then
    usage
fi

# Parameters
count="${1}"
shift 1

if [ "${count}" -le "0" ];
then
    echo "${0}: count must be a non-negative integer"
    usage
fi

# Programs
ACT=${ACT="act"}

dir_name="fuzz_$(date '+%y_%m_%d_%H_%M_%S')"
mkdir "${dir_name}"

for file in "${@}";
do
    fuzz_file "${ACT}" "${dir_name}" "${file}" "${count}"
done

symlink_name="_fuzz_latest"
rm -f "${symlink_name}"
ln -s "${dir_name}" "${symlink_name}"

open_directory "${dir_name}"