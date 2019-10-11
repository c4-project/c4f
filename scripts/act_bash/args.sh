#!/usr/bin/env bash
#
# Helpers for keeping a consistent argument set across the various ACT
# scripts.
#
# Expects log.sh to be sourced.

declare BACKEND
declare COMPILER
declare DUNE_EXEC
declare VERBOSE


# Sets a variable, but errors if the value is an empty ('zero') string.
#
# Globals:
#   !2: write
#
# Arguments:
#   1: a human-readable variable name, used in errors.
#   2: the name of the variable, used to set indirectly.
#   3: the value to store in ${!2}.
act::set_nz() {
  local hr_name="$1"
  local name="$2"
  local value="$3"

  if [[ -z ${value} ]]; then
    act::error "can't set ${hr_name} to ''"
    exit
  fi
  printf -v "${name}" '%s' "${value}"
}


# Sets a variable, but errors if the value is an empty ('zero') string or if
# the variable already has a non-empty ('nonzero') value.
#
# Globals:
#   !2: read, write
#
# Arguments:
#   1: a human-readable variable name, used in errors.
#   2: the name of the variable, used to set indirectly.
#   3: the value to store in ${!2}.
act::set_nz_once() {
  local hr_name="$1"
  local name="$2"
  local value="$3"

  if [[ -n ${!name} ]]; then
    act::error "${hr_name} already set"
    exit
  fi
  act::set_nz "${hr_name}" "${name}" "${value}"
}


# Checks to see if DUNE_EXEC is set; if so, logs the fact.
#
# Globals:
#   DUNE_EXEC: read
#   VERBOSE: read (transitively)
act::check_dune_exec() {
   if [[ ${DUNE_EXEC} == "true" ]]; then
    act::log "%s: using 'dune exec' for ACT.\n" "$0"
  fi
}


# Composes a block of flags that, when sent to an ACT shell script that
# accepts the `q` (quiet) or `v` (verbose) flag, replicates the current
# verbosity.
#
# Globals:
#   VERBOSE: read
act::flags_qv() {
  local flags="-q"
  if [[ ${VERBOSE} == "true" ]]; then flags="-v"; fi
  echo "${flags}"
}


# Composes a block of flags that, when sent to an ACT shell script that
# accepts the `q` (quiet), `v` (verbose), and `x` (dune-exec) flags,
# replicates the current verbosity and execution behaviour
#
# Globals:
#   DUNE_EXEC: read
#   VERBOSE: read
act::flags_qvx() {
  local flags
  flags="$(act::flags_qv)"
  if [[ ${DUNE_EXEC} == "true" ]]; then flags="${flags}x"; fi
  echo "${flags}"
}


# Runs the program in argument 1 with the quiet, verbose, and dune-exec
# flags set, and the arguments from position 2 onwards.
#
# Globals:
#   DUNE_EXEC: read (transitively)
#   VERBOSE: read (transitivtly)
#
# Arguments:
#   1. The program to run.
#   2+. Extra arguments to supply to the program.
act::run_with_qvx() {
  local prog="$1"
  shift

  "${prog}" "$(act::flags_qvx)" "$@"
}


# Runs the program in argument 1 with the quiet, verbose, and dune-exec
# flags set, '-b BACKEND' and '-c COMPILER', and the arguments from position 2
# onwards.
#
# Globals:
#   BACKEND: read
#   COMPILER: read
#   DUNE_EXEC: read (transitively)
#   VERBOSE: read (transitivtly)
#
# Arguments:
#   1. The program to run.
#   2+. Extra arguments to supply to the program.
function act::run_with_bcqvx() {
  local prog="$1"
  shift

  act::run_with_qvx "${prog}" -b "${BACKEND}" -c "${COMPILER}" "$@"
}
