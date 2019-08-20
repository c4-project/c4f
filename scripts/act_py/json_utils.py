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
"""JSON utilities."""

import dataclasses
import json
import os
import typing


def dump_dataclass(item: typing.Any, fp: typing.TextIO) -> None:
    """Dumps a dataclass to a file.

    :param item: The dataclass to dump to a file.
    :param fp: A file-like object to use as the target of the write.
    :return: Nothing.
    """
    json.dump(dataclasses.asdict(item), fp, indent="\t")


class FieldMissingException(Exception):
    """Exception raised when conversion from a dict (eg. from JSON) depends on a field that isn't present.

    This is usually used as a fancier, more semantic wrapper over KeyError.
    """

    context: str
    field_name: str

    def __init__(self, field_name: str, context: str):
        """Constructs a `FieldMissingException`.
        :param field_name:  The name of the missing field.
        :param context:  The name of the 'thing' being converted from a dictionary.
        """
        self.field_name = field_name
        self.context = context

    def __str__(self):
        return f"Expected field missing in {self.context}: '{self.field_name}'"


def field(
    d: typing.Mapping[str, typing.Any], key: str, context: str = "dict"
) -> typing.Any:
    """Tries to get the given key from the dictionary, but throws a
    slightly more semantically pleasing exception if it doesn't exist.

    :param d: The dictionary to inspect.
    :param key: The key to get.
    :param context: If given, the name of the 'thing' being converted from a dictionary;
      used in any exceptions raised.
    :return: The field named by `key` in `d` if it exists.
    """
    try:
        return d[key]
    except KeyError as k:
        raise FieldMissingException(key, context) from k


def bool_field(
    d: typing.Mapping[str, typing.Any], key: str, context: str = "dict"
) -> bool:
    """As `field`, but converts to `bool`.

    :param d: The dictionary to inspect.
    :param key: The key to get.
    :param context: If given, the name of the 'thing' being converted from a dictionary;
      used in any exceptions raised.
    :return: The field named by `key` in `d` if it exists.
    """
    return bool(field(d, key, context))


def str_field(
    d: typing.Mapping[str, typing.Any], key: str, context: str = "dict"
) -> str:
    """As `field`, but converts to `str`.

    :param d: The dictionary to inspect.
    :param key: The key to get.
    :param context: If given, the name of the 'thing' being converted from a dictionary;
      used in any exceptions raised.
    :return: The field named by `key` in `d` if it exists.
    """
    return str(field(d, key, context))


class ExtendedEncoder(json.JSONEncoder):
    """JSON encoder that understands pathlike and dataclass objects."""

    def default(self, obj):
        if isinstance(obj, os.PathLike):
            return os.fspath(obj)
        elif dataclasses.is_dataclass(obj) and not isinstance(obj, type):
            return dataclasses.asdict(obj)
        # Let the base class default method raise the TypeError
        return json.JSONEncoder.default(self, obj)
