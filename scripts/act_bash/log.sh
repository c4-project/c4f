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
#   2: if given, the exit code to use.  Defaults to 1.
act::fatal() {
  act::error "$1"
  exit "${2:-1}"
}

# Echoes the argument to stderr, prefixed by the program name, then
# calls `usage`.
#
# Arguments:
#   1: the string to echo.
act::arg_error() {
  act::error "$1"
  echo >&2
  usage >&2
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

