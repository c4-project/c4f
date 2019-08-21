# The Automagic Compiler Tormentor
#
# Copyright (c) 2018--2019 Matt Windsor and contributors
#
# ACT itself is licensed under the MIT License. See the LICENSE file in the
# project root for more information.
#
# ACT is based in part on code from the Herdtools7 project
# (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
# project root for more information.

import argparse

# A parser that contains a positional argument for taking an auxiliary JSON file.
aux_in_parser: argparse.ArgumentParser = argparse.ArgumentParser(add_help=False)
aux_in_parser.add_argument(
    "aux",
    type=argparse.FileType("r"),
    help="Path to the auxiliary JSON file created during a previous delitmus pass.",
)

# A parser that contains the common set of arguments used in the test-handling
# scripts (run_test, check_test).
test_phase_parser: argparse.ArgumentParser = argparse.ArgumentParser(add_help=False)
test_phase_parser.add_argument(
    "test", type=argparse.FileType("r"), help="The input test file."
)
log_group = test_phase_parser.add_mutually_exclusive_group()
log_group.add_argument(
    "--log",
    type=str,
    default="WARNING",
    help="the log level (eg. DEBUG, INFO, WARNING, ERROR, or CRITICAL)",
)
log_group.add_argument(
    "-q",
    "--quiet",
    action="store_const",
    const="ERROR",
    dest="log",
    help="quiet (short for --log=ERROR)"
)
log_group.add_argument(
    "-v",
    "--verbose",
    action="store_const",
    const="INFO",
    dest="log",
    help="verbose (short for --log=INFO)"
)
