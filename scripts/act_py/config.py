"""A Python approximation of the act.conf AST.

This should be kept in sync with the main AST in ACT proper.
"""

#  The Automagic Compiler Tormentor
#  Copyright (c) 2018--2020 Matt Windsor and contributors.
#  - ACT itself is licensed under the MIT License. See the LICENSE file in the
#    project root for more information.
#  - ACT is based in part on code from the Herdtools7 project
#    (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
#    project root for more information.

import enum
from dataclasses import dataclass

import typing
from act_py import act_id


class Target(enum.Enum):
    """Enumeration of target IDs supported in act.conf 'arch' directives."""

    X64_ATT = "x64.att"
    X86_ATT = "x86.att"
    PPC64_LE = "ppc64.le"

    @property
    def short_name(self) -> str:
        """The most significant part of the target's ID."""
        (sn, _, _) = self.value.partition(".")
        return sn

    def force_32bit(self, arch: str) -> bool:
        """Tests whether 32-bit mode needs to be forced in a compiler."""
        return self is Target.X86_ATT and arch == "x86_64"


@dataclass
class Compiler:
    """Information about a compiler on an ACT target machine."""

    style: str
    target: Target
    cmd: str
    args: typing.List[str]

    @property
    def args_str(self) -> str:
        return " ".join((f'"{a}"' for a in self.args))

    def print(self) -> None:
        print(f"        style {self.style}")
        print(f"        arch  {self.target.value}")
        print(f'        cmd   "{self.cmd}"')
        print(f"        argv  {self.args_str}")


@dataclass
class Flag:
    """Information about a fuzzer flag setting."""

    wins: int
    losses: int

    def __str__(self) -> str:
        """Converts the flag to a string representation.

        :return: The flag as it would appear in a config file.
        """
        if self.wins == 0:
            return "false"
        elif self.losses == 0:
            return "true"
        else:
            return f"ratio {self.wins}:{self.losses}"


def exact_flag(value: bool) -> Flag:
    """Constructs a flag that will always return the given boolean when evaluated.

    >>> str(exact_flag(True))
    'true'

    >>> str(exact_flag(False))
    'false'

    :param value: The boolean to return.
    :return: An 'exact' flag over `value`.
    """
    return Flag(1, 0) if value else Flag(0, 1)


@dataclass
class Fuzz:
    """Information about the fuzzer configuration."""

    # Integer parameters.
    params: typing.Mapping[act_id.Id, int]

    # Boolean flags.
    flags: typing.Mapping[act_id.Id, Flag]

    def print(self) -> None:
        """Dumps this fuzzer configuration to stdout.

        :return: Nothing.
        """
        print("fuzz {")
        for (pk, pv) in self.params.items():
            print(f"    set param {pk} to {pv}")
        for (fk, fv) in self.flags.items():
            print(f"    set flag  {fk} to {fv}")
        print("}")


@dataclass
class Backend:
    """Information about a backend on an ACT target machine."""

    style: str
    cmd: str
    c_model: typing.Optional[str]

    def print(self) -> None:
        print(f"        style   {self.style}")
        print(f'        cmd     "{self.cmd}"')
        if self.c_model is not None:
            print(f'        c_model "{self.c_model}"')


@dataclass
class Machine:
    """Information about an ACT target machine."""

    id: str
    arch: str
    compilers: typing.Mapping[str, Compiler]
    backends: typing.Mapping[str, Backend]

    def print_compilers(self) -> None:
        for (key, val) in self.compilers.items():
            print(f"    compiler {key} {{")
            val.print()
            print("    }")

    def print_backends(self) -> None:
        for (key, val) in self.backends.items():
            print(f"    backend {key} {{")
            val.print()
            print("    }")

    def print(self) -> None:
        print(f"machine {self.id} {{")
        print("    via local")
        self.print_compilers()
        self.print_backends()
        print("}")
