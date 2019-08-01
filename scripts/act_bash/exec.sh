#!/usr/bin/env bash
#
# Contains a wrapper for running ACT binaries, which allows for the use
# of 'dune exec'.

# If `DUNE_EXEC` is set to "true", calls to `act::exec` will run their program
# through `dune exec`.
declare DUNE_EXEC

# If set, overrides the choice of main `act` executable
declare ACT


# Runs an OCaml ACT tool.
#
# If `DUNE_EXEC` is set to "true", the tool will be run indirectly through
# `dune exec`.
#
# Globals:
#   - DUNE_EXEC (read)
#
# Arguments:
#   1: the ACT program to execute.
#   *: the arguments to the program.
act::exec() {
  local prog=$1
  shift 1

  if [[ DUNE_EXEC = "true" ]]; then
    dune exec "${prog}" -- "$@"
  else
    "${prog}" "$@"
  fi
}

# Runs the 'act-backend' tool.
#
# Globals:
#   - ACT (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::backend() {
  act::exec "${ACT:-"act"}" backend "$@"
}

# Runs the 'act-delitmus' tool.
#
# Globals:
#   - ACT (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::delitmus() {
  act::exec "${ACT:-"act-c"}" delitmus "$@"
}
