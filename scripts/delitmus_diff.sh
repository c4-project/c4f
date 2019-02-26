#!/bin/sh
#
# Given a compiler ID and memalloy-style output directory, this script
# runs `act explain` on both the litmus file and its parallel C witness, and
# diffs the results.
#
# This is intended to show differences in the assembly output between
# the C witness and its delimusification, and therefore highlight bugs in the
# delitmusifier.

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

# $1: name of pass
# $2: compiler
# $3: base name
# $4: output file
# $5: input file
# All other arguments: extra flags passed to act
explain () {
	pass="${1}"
	compiler="${2}"
	bname="${3}"
	output="${4}"
	shift 4 # input is, therefore, $1, and included in $@

	if ${ACT} explain -compiler "${compiler}" "${@}" > "${output}";
	then
		if [ "$(wc -l <"${output}")" -eq 0 ];
		then
			>&2 echo "$0: ${pass} on ${bname} returned empty file"
			return 2
		fi
		return 0
	else
		>&2 echo "$0: ${pass} on ${bname} failed"
		return 1
	fi
}

for file in "${c_dir}"/*.c;
do
	bname=$(basename "${file}" | sed 's/.c$//')
	litmus="${l_dir}/${bname}.litmus"

	delitmus_ofile=$(mktemp -t "${bname}.XXXXXX") || exit 1
	if explain "delitmus" "${compiler}" "${bname}" "${delitmus_ofile}" "${litmus}";
	then
		# Get C variables from litmus test in comma-separated form.
		cvars=$(${ACT} c explain -dump-vars "${litmus}" | tr -s '\n' ',' | sed 's/,$//')

		direct_ofile=$(mktemp -t "${bname}.XXXXXX") || exit 1
		if explain "direct-compile" "${compiler}" "${bname}" "${direct_ofile}" "${file}" -cvars "${cvars}";
		then
			diff -u "${direct_ofile}" "${delitmus_ofile}"
		fi
	fi
done
