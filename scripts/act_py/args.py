# The Automagic Compiler Tormentor
#
# Copyright (c) 2018--2019 Matt Windsor and contributors
#
# ACT itself is licensed under the MIT License. See the LICENSE file in the
# project root for more information.
#
# ACT is based in part on code from the Herdtools7 project
# (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
# project root for more information. *)

import argparse

"""A parser that contains a positional argument for taking an auxiliary JSON file."""
aux_in_parser: argparse.ArgumentParser = argparse.ArgumentParser(add_help=False)
aux_in_parser.add_argument(
    "aux",
    type=argparse.FileType("r"),
    help="Path to the auxiliary JSON file created during a previous delitmus pass.",
)
