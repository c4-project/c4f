#!/usr/bin/env bash
# Logging functionality.

declare VERBOSE

# If VERBOSE is "true", sends all subsequent arguments to printf.
#
# Globals:
#   - VERBOSE (read)
#
# Arguments:
#   1+: the arguments to send to printf if VERBOSE is set.
act::log () {
  if [[ "${VERBOSE}" = "true" ]]; then
    # shellcheck disable=SC2059
    printf "${@}"
  fi
}

