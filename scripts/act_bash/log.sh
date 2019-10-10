#!/usr/bin/env bash
# Logging functionality.

# Logging is governed by the `VERBOSE` variable, which should be set to
# "true" to make calls to `act::log` do things.
#
# `VERBOSE` may be set readonly once its final value is stable.
declare VERBOSE

# Echoes the argument to stderr, prefixed by the program name.
#
# Arguments:
#   1: the string to echo.
act::error() {
  echo "$0: $1" >&2
}

# Echoes the argument to stderr, prefixed by the program name, then
# exits.
#
# Arguments:
#   1: the string to echo.
act::fatal() {
  act::error "$1"
  exit
}

# Echoes the argument to stderr, prefixed by the program name, then
# calls `usage`.
#
# Arguments:
#   1: the string to echo.
act::arg_error() {
  act::error "$1"
  usage
}

# If VERBOSE is "true", sends all subsequent arguments to printf.
#
# Globals:
#   - VERBOSE (read)
#
# Arguments:
#   1+: the arguments to send to printf if VERBOSE is set.
act::log() {
  if [[ "${VERBOSE}" = "true" ]]; then
    # shellcheck disable=SC2059
    printf "${@}" >&2
  fi
}

