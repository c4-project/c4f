#!/usr/bin/env bash
#
# Common constants and glue code for scripts using Memalloy.


## Constants ##


# TODO(@MattWindsor91): most of these shouldn't be hardcoded.

# The model, relative to Memalloy's models directory, to use to generate
# violations.
readonly MEMALLOY_VIOLATES_MODEL="c11_lahav.cat"

# The model, relative to Memalloy's models directory, to use to filter
# read-modify-writes.
readonly MEMALLOY_NORMWS_MODEL="c11_normws.cat"

# The number of events Memalloy should be asked to produce by default.
readonly DEFAULT_MEMALLOY_EVENTS=4

