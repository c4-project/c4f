#!/usr/bin/env bash
#
# Contains functions that generate systematic names for ACT artefacts.
# Useful for finding out where one ACT script put a specific file, and so on.
#
# Expects log.sh to be sourced.


## Used globals ##


# Populated by `act::setup_temp_output_dir`.
declare OUTPUT_DIR


## Functions ##


# Outputs the base prefix of a systematically generated fuzzer output name.
#
# Arguments:
#   1: name to transform
act::fuzz_base_name() {
  local in_file="${1}"

  basename "${in_file}" .litmus | tr '.' '_'
}


# Outputs the systematically generated fuzzer output name.
#
# Arguments:
#   1: name to transform
#   2: number in the sequence of fuzzer outputs
act::fuzz_name() {
  local in_file="${1}"
  local nth="${2}"

  printf "%s_%d.litmus" "$(act::fuzz_base_name "${in_file}")" "${nth}"
}


# Populates OUTPUT_DIR with a temporary directory then sets a callback to
# remove it when the shell dies.
#
# Globals:
#   OUTPUT_DIR: write (and reference in trap)
act::setup_temp_output_dir() {
  OUTPUT_DIR="$(mktemp -d)" || exit 2
  act::log "using temporary directory: '%s'.\n" "${OUTPUT_DIR}"
  trap 'rm -rf ${OUTPUT_DIR}' EXIT
}

