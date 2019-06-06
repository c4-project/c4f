#!/bin/sh
#
# Automatically generates an act.conf for the local machine and puts it on stdout.

host="$(hostname -s)"
if [ -z "${host}" ];
then
	host="localhost"
fi

raw_arch="$(uname -m)"
case "${raw_arch}" in
x86_64 )
	arch="x86"
	is_64=1
;;
* )
	echo "Unsupported architecture: ${raw_arch}" >&2
	exit 1
;;
esac

# Emits the appropriate 'arch' ID for a GCC-like compiler running on this
# machine.
gcc_like_arch ()
{
	case "${arch}" in
	x86 )
		echo "x86.att"
	;;
	* )
		echo "unknown"
	;;
	esac
}

# Tries to emit a compiler stanza for a GCC-like compiler.
#
# $1: the compiler command
gcc_like ()
{
	_compiler="${1}"
	"${_compiler}" --version >/dev/null 2>&1 || exit

	for _optlevel in "0" "3";
	do
		printf '\n    compiler %s.%s.O%d {\n' "${_compiler}" "${arch}" "${_optlevel}"
		printf '        emits "%s" \n' "$(gcc_like_arch)"
		printf '        cmd   "%s" \n' "${_compiler}"

		printf "        argv  "
		[ -z "${is_64}" ] || printf '"-m32" '
		[ "${_optlevel}" -eq "0" ] || printf '"-O%d" ' "${_optlevel}"
		echo '"-DNO_PTHREADS"'

		echo "    }"
	done
}


echo "# Auto-generated act.conf for '${host}' (architecture ${raw_arch})."
echo "# Generated on $(date)."
echo
echo "machine ${host} {"
echo "    via local"

# Compilers
gcc_like "gcc"
gcc_like "clang"

# Simulators
# TODO(@MattWindsor91)

echo "}"
