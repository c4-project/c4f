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

import re
import typing
from dataclasses import dataclass


@dataclass
class Lid:
    """A split representation of a Litmus ID."""

    tid: typing.Optional[int]
    var: str

    @property
    def qualified(self) -> str:
        """Converts the Litmus ID to a C ID in the memalloy-style 't$n$v' format.

        >>> Lid(0, "r0").qualified
        't0r0'

        """
        return self.var if self.tid is None else f"t{self.tid}{self.var}"


def parse(id_str: str) -> Lid:
    """Parses a string as a Litmus ID.

    >>> parse("0:r0").tid
    0
    >>> parse("0:r0").var
    'r0'
    >>> parse("x").tid

    >>> parse("x").var
    'x'

    :param id_str: The identifier string to parse.
    :return: A Litmus ID object corresponding to the input string.
    """
    (l, colon, r) = id_str.partition(":")
    tid: typing.Optional[int] = None if colon == "" else int(l)
    var: str = l if colon == "" else r
    return Lid(tid, var)


local_re: typing.Pattern[str] = re.compile(
    r"(?P<tid>[0-9]+):(?P<var>[_a-zA-Z]\w*)", re.ASCII
)


def rewrite_post_locals(post_str: str, op: typing.Callable[[Lid], str]) -> str:
    """Rewrites each instance of a local litmus ID in a postcondition using
       the given callback.

    >>> rewrite_post_locals('exists (0:a == 6 /\\ 1:b == 4 /\\ x == 5)', lambda x: x.qualified)
    'exists (t0a == 6 /\\\\ t1b == 4 /\\\\ x == 5)'
    """
    return local_re.sub(
        lambda match: op(Lid(int(match["tid"]), match["var"])), post_str
    )
