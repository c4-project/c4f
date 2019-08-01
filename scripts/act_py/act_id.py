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
"""Support for working with ACT IDs."""

import typing

# Type of act IDs.
Id = typing.NewType("Id", str)


def to_dir(identifier: Id) -> str:
    """Converts an ACT ID to a format safe for use as a path component.

    The focus is on bijectivity rather than human-readability.

    Example:

    >>> to_dir(Id("spam_spam_spam.sausage.eggs/ham.and\\spam"))
    'spam__spam__spam_Dsausage_Deggs_Fham_Dand_Bspam'

    :param identifier:
        The ID to convert.
    :return:
        The converted ID.
    """
    directory: str = identifier
    for (f, t) in id_to_dir_replacements:
        directory = directory.replace(f, t)
    return directory


id_to_dir_replacements: typing.List[typing.Tuple[str, str]] = [
    ("_", "__"),
    ("/", "_F"),
    ("\\", "_B"),
    (".", "_D"),
]


def is_blank(s : str) -> bool:
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
    return s.strip() == ''

def qualify(machine_id: Id, other_id: Id) -> Id:
    """Fully-qualifies a machine-dependent ID by appending it to a machine ID.

    Examples:

    >>> qualify(Id("localhost"), Id("gcc.x86.O3"))
    'localhost.gcc.x86.O3'

    >>> qualify(Id(""), Id("gcc.x86.O3"))
    'gcc.x86.O3'

    :param machine_id: The machine identifier.
    :param other_id: The identifier to qualify with `machine_id`.
    :return: The qualified identifier.
    """
    return other_id if machine_id.strip() == '' else Id(".".join([machine_id, other_id]))
