#!/usr/bin/env python3
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

import act_py
import act_py.auxfile
import act_py.args
import act_py.litmus_id

# TODO(@MattWindsor91): fix up ACT so that this isn't necessary.

if __name__ == '__main__':
    parser = argparse.ArgumentParser(parents=[act_py.args.aux_in_parser])
    args = parser.parse_args()

    with open(args.aux) as f:
        aux = act_py.auxfile.load(f)

    aux.rewrite_locals(lambda x: x.qualified)

    with open(args.aux, mode='w') as fp:
        aux.dump(fp)

