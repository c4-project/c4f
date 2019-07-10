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
"""JSON utilities."""

import dataclasses
import json
import os


class ExtendedEncoder(json.JSONEncoder):
    """JSON encoder that understands pathlike and dataclass objects."""
    def default(self, obj):
        if isinstance(obj, os.PathLike):
            return os.fspath(obj)
        elif dataclasses.is_dataclass(obj) and not isinstance(obj, type):
            return dataclasses.asdict(obj)
        # Let the base class default method raise the TypeError
        return json.JSONEncoder.default(self, obj)
