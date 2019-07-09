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
"""Miscellaneous I/O utility functions."""


import os


def try_mkdir(dir_path: str) -> None:
    """Tries to make a directory; silently succeeds if it already exists.

    :param dir_path: The path to make, if possible.
    """
    try:
        os.mkdir(dir_path)
    except FileExistsError as e:
        if not os.path.isdir(dir_path):
            raise e


def check_file_exists(file_path: str) -> None:
    """Checks that the given file path exists.
    Raises `FileNotFoundError` if not.

    :param file_path: The path of the subject to test.
    """
    if not os.path.isfile(file_path):
        raise FileNotFoundError(file_path)
