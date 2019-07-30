#!/usr/bin/env bash
#
# Contains functions that generate systematic names for ACT artefacts.
# Useful for finding out where one ACT script put a specific file, and so on.

# Outputs the base prefix of a systematically generated fuzzer output name.
#
# Arguments:
#   1: name to transform
act::fuzz_base_name() {
  local in_file="${1}"

  basename "${in_file}" .litmus | tr '.' '_'
}

# Outputs the a systematically generated fuzzer output name.
#
# Arguments:
#   1: name to transform
#   2: number in the sequence of fuzzer outputs
act::fuzz_name() {
  local in_file="${1}"
  local nth="${2}"

  printf "%s_%d.litmus" "$(act::fuzz_base_name "${in_file}")" "${nth}"
}

