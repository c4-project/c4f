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
"""Utilities for string manipulation."""


import typing


def is_blank(s: str) -> bool:
    """Returns true provided that the input string is empty after stripping whitespace.

    Examples:

    >>> is_blank("")
    True

    >>> is_blank(" ")
    True

    >>> is_blank("     \t\t   ")
    True

    >>> is_blank("     spam    ")
    False

    :param s: The string to query.
    :return: Whether `s` is empty after stripping whitespace.
    """
    return s.strip() == ""


def with_default_if_blank(s: typing.Optional[str], *, default: str) -> str:
    """Substitutes a default for a string if that string is blank.

    :param s: The string to use if non-blank.
    :param default: The string to use if `s` is blank.
    :return: `default` if `is_blank(s)` is true; `s` otherwise.
    """
    if s is None:
        return default
    if is_blank(s):
        return default
    return s
