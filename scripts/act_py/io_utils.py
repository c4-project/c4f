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
"""Miscellaneous I/O utility functions."""
import logging
import pathlib


def write_text(out_file: pathlib.Path, contents: str) -> None:
    """Writes a whole string to a file in one go.

    :param out_file: The path of the file to write to (truncating).
    :param contents: The contents to write to the file.
    """
    with out_file.open("w") as fp:
        fp.write(contents)


def check_file_exists(file_path: pathlib.Path) -> None:
    """Checks that the given file path exists.
    Raises `FileNotFoundError` if not.

    :param file_path: The path of the file to test.
    """
    if not file_path.is_file():
        raise FileNotFoundError(file_path)


def remove_if_exists(file_path: pathlib.Path) -> None:
    """Tries to remove the given path, but silently succeeds if it doesn't
    exist.

    :param file_path: The path of the file to remove, if it exists.
    """
    try:
        file_path.unlink()
    except FileNotFoundError:
        pass


def config_logger(loglevel: str) -> None:
    """Sets up the logger using the given log level.

    :param loglevel: The log level specified on the command line.
    """
    numeric_level = getattr(logging, loglevel.upper(), None)
    if not isinstance(numeric_level, int):
        raise ValueError("Invalid log level: %s" % loglevel)
    logging.basicConfig(level=numeric_level)
