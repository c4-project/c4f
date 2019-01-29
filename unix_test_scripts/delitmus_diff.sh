#!/bin/sh
#
# Given a compiler ID and memalloy-style output directory, this script
# runs `act explain` on both the litmus file and its parallel C witness, and
# diffs the results.
#
# This is intended to show differences in the assembly output

if [ "$#" -ne "2" ];
then
	echo "Usage: $0 COMPILER MEMALLOY_DIR"
	exit
fi

# Parameters
compiler="${1}"
root_dir="${2}"

# Programs
ACT=${ACT="act"}
DIFF=${DIFF="diff"}

c_dir="${root_dir}/C"
l_dir="${root_dir}/litmus"

for file in $(find "${c_dir}" -name '*.c');
do
	bname=$(basename "${file}" | sed 's/.c$//')
	litmus="${l_dir}/${bname}.litmus"

	delitmus_ofile=$(mktemp -t "${bname}.XXXXXX") || exit 1
	${ACT} explain -compiler "${compiler}" "${litmus}" > delitmus_ofile
	if [ $? -ne 0 ]; then
		>&2 echo "$0: delitmus on ${bname} failed, skipping"
		continue
	fi
	if [ $(wc -l <"${delitmus_ofile}") -eq 0 ]; then
		>&2 echo "$0: delitmus on ${bname} returned empty file, skipping"
		continue
	fi

	# TODO: extract C variables and pass in as -cvars.
	direct_ofile=$(mktemp -t "${bname}.XXXXXX") || exit 1
	${ACT} explain -compiler "${compiler}" "${file}" > direct_ofile
	if [ $? -ne 0 ]; then
		>&2 echo "$0: direct compilation on ${bname} failed, skipping"
		continue
	fi
	if [ $(wc -l <"${direct_ofile}") -eq 0 ]; then
		>&2 echo "$0: direct compilation on ${bname} returned empty file, skipping"
		continue
	fi
	
	diff -u "${direct_ofile}" "${delitmus_ofile}"
done
