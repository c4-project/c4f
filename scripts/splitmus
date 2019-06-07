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
LITMUS=${LITMUS="litmus7"}

# Prints the self-reported name of an assembly Litmus test to stdout.
#
# $1: path of Litmus file
get_litmus_name()
{
	# The first line of a Litmus test is 'ARCH NAME', where ' ' is just
	# a regular space, so we can get the name of the test from that.
	#
	# TODO(@MattWindsor91): make act able to do this sort of litmus-test
	#                       header reading.
	infile="${1}"
	head -n1 "${infile}" | cut -d ' ' -f 2
}

# Runs the strip-litmus Awk script on the given file.
#
# $1: name of file to strip
# $2: the word to substitute for each litmus program body, for example
#     'mfence' on x86
strip_litmus()
{
	_infile="${1}"
	_blank="${2}"

	${AWK} -f "${SCRIPTDIR}"/strip_litmus.awk -v blank="${_blank}" "${_infile}"

	unset _infile _blank
}

# Runs the splicing Awk script on the given stub and C files; dumps the results
# to stdout.
#
# $1: path to stub file
# $2: path to litmus C file
splice()
{
	_stubfile="${1}"
	_cfile="${2}"

	${AWK} -f "${SCRIPTDIR}"/splice.awk "${_stubfile}" "${_cfile}"

	unset _stubfile _cfile
}

# Makes a temporary file and emits the assembly stubs for the given test into it.
#
# $1: path to input file
# $2: input file's Litmus name
# $3: compiler (TO BE REMOVED)
make_stubs()
{
	_infile="${1}"
	_name="${2}"
	_compiler="${3}"

	_stubtmpl="act-${_name}-XXXXXX.s.stubs"
	_stubfile="$(mktemp "${_stubtmpl}")"
	"${ACT}" asm gen-stubs -compiler "${_compiler}" "${_infile}" > "${_stubfile}"
	echo "${_stubfile}"

	unset _infile _name _compiler _stubtmpl _stubfile
}

# Makes a temporary file and emits the assembly litmus for the given test into
# it.
#
# $1: path to input file
# $2: input file's Litmus name
# $3: compiler (TO BE REMOVED)
make_asm_litmus()
{
	_infile="${1}"
	_name="${2}"
	_compiler="${3}"

	_slittmpl="act-${_name}-XXXXXX.s.litmus"
	_slitfile="$(mktemp "${_slittmpl}")"
	"${ACT}" asm litmusify -compiler "${_compiler}" "${_infile}" > "${_slitfile}"
	echo "${_slitfile}"

	unset _infile _name _compiler _slittmpl _slitfile
}

# Makes a temporary file and emits a stripped version of the given litmus file
# into it.
#
# $1: path to litmus file to strip
# $2: input file's Litmus name
# $3: ASM instruction to use to pad out stripped files
make_stripped_litmus()
{
	_slitfile="${1}"
	_name="${2}"
	_blank="${3}"

	_striptmpl="act-${_name}-XXXXXX-stripped.s.litmus"
	_stripfile="$(mktemp "${_striptmpl}")"
	strip_litmus "${_slitfile}" "${_blank}" > "${_stripfile}"

	echo "${_stripfile}"

	unset _slitfile _name _blank _striptmpl _stripfile
}

# Makes a temporary directory and invokes litmus7 to create an environment for
# running litmus tests in it.
#
# $1: path to litmus file for which we want to generate an environment
# $2: input file's Litmus name
make_litmus_environment()
{
	_stripfile="${1}"
	_name="${2}"

	_littmpl="litmus-${_name}-XXXXXX"
	_litdir="$(mktemp -d "${_littmpl}")"
	"${LITMUS}" "${_stripfile}" -o "${_litdir}"

	echo "${_litdir}"

	unset _stripfile _name _littmpl _litdir
}

# Prints the script's usage and exits.
usage()
{
	echo "usage: ${0} COMPILER FILE"
	exit 1
}


# Parameters
[ "$#" -ne "2" ] && usage
compiler="${1}"
infile="${2}"

name=$(get_litmus_name "${infile}")


# TODO(@MattWindsor91): this'll need changing once we finally get act to
# take assembly as input.
stubfile="$(make_stubs "${infile}" "${name}" "${compiler}")"
slitfile="$(make_asm_litmus "${infile}" "${name}" "${compiler}")"
stripfile="$(make_stripped_litmus "${slitfile}" "${name}" "mfence")"
litdir="$(make_litmus_environment "${stripfile}" "${name}")"

cfile="${litdir}/$(echo "${stripfile}" | sed 's/.litmus$/.c/')"
ncfile="$(mktemp "act-${name}-XXXXXX.c")"
splice "${stubfile}" "${cfile}" > "${ncfile}"
mv "${ncfile}" "${cfile}"

echo "${litdir}"

rm "${stripfile}"
rm "${stubfile}"
rm "${slitfile}"
