#!/usr/bin/env bash
#
# Contains a wrapper for running ACT binaries, which allows for the use
# of 'dune exec'.


# If `DUNE_EXEC` is set to "true", calls to `act::exec` will run their program
# through `dune exec`.
declare DUNE_EXEC

# If set, overrides the choice of `act-asm` executable.
declare ACT_ASM

# If set, overrides the choice of `act-backend` executable.
declare ACT_BACKEND

# If set, overrides the choice of `act-c` executable.
declare ACT_C

# If set, overrides the choice of `act-fuzz` executable.
declare ACT_FUZZ

# If set, overrides the choice of `act-state` executable.
declare ACT_STATE


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

  if [[ ${DUNE_EXEC} = "true" ]]; then
    # We can't build here because some of the act scripts fork off multiple
    # act tool executions, and building on each would cause race conditions.
    dune exec --no-build --display=quiet "${prog}" -- "$@"
  else
    "${prog}" "$@"
  fi
}


# Runs the ACT 'backend' tool.
#
# Globals:
#   - ACT_BACKEND (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::backend() {
  act::exec "${ACT_BACKEND:-"act-backend"}" "$@"
}


# Runs the ACT 'c' tool.
#
# Globals:
#   - ACT_C (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::c() {
  act::exec "${ACT_C:-"act-c"}" "$@"
}


# Runs the ACT 'compile' sub-tool.
#
# Globals:
#   - ACT (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::compile() {
  # `compile` is currently `act-c compile`; this may change later.
  act::c compile "$@"
}


# Runs the ACT 'delitmus' sub-tool.
#
# Globals:
#   - ACT_C (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::delitmus() {
  # `delitmus` is currently `act-c delitmus`; this may change later.
  act::c delitmus "$@"
}


# Runs the ACT 'fuzz' tool.
#
# Globals:
#   - ACT_FUZZ (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::fuzz() {
  act::exec "${ACT_FUZZ:-"act-fuzz"}" "$@"
}


# Runs the ACT 'litmusify' sub-tool.
#
# Globals:
#   - ACT (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::litmusify() {
  act::exec "${ACT_ASM:-"act-asm"}" litmusify "$@"
}


# Runs the ACT 'state' tool.
#
# Globals:
#   - ACT_STATE (read)
#   - DUNE_EXEC (transitively read)
#
# Arguments:
#   *: the arguments to the program.
act::state() {
  act::exec "${ACT_STATE:-"act-state"}" "$@"
}

