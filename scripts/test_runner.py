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
import collections
import typing
import subprocess
from dataclasses import dataclass


def make_predicate_args(
    machine_predicate: typing.Optional[str], compiler_predicate: typing.Optional[str]
) -> typing.Iterator[str]:
    """Yields an argument vector for invoking an act command with the given machine and compiler predicate.

    >>> [ x for x in make_predicate_args(None, None) ]
    []

    >>> [ x for x in make_predicate_args('(spam spam sausage)', '(eggs ham)') ]
    ['-machine-predicate', '(spam spam sausage)', '-compiler-predicate', '(eggs ham)']

    :param machine_predicate: An optional predicate to supply to the command to filter machines.
    :param compiler_predicate: An optional predicate to supply to the command to filter compilers.
    :return: An iterator of act command arguments.
    """
    if machine_predicate is not None:
        yield "-machine-predicate"
        yield machine_predicate
    if compiler_predicate is not None:
        yield "-compiler-predicate"
        yield compiler_predicate


def parse_compiler_list(
    lines: typing.Iterable[str]
) -> typing.Dict[str, typing.Set[str]]:
    """
    Parses a compiler information list from `act configure list-compilers`.

    >>> parse_compiler_list([ 'localhost gcc.x86.O0',
    ...                       'localhost gcc.x86.O3',
    ...                       'localhost clang.x86.O3',
    ...                       'farfaraway msvc.x86.O3' ]) == {
    ... 'localhost': {'gcc.x86.O0', 'gcc.x86.O3', 'clang.x86.O3'}, 'farfaraway': {'msvc.x86.O3'}}
    True

    :param lines: Iterable of lines in the compiler list.
    :return: A dictionary mapping machine IDs to sets of compiler IDs.
    """
    machines: typing.DefaultDict[str, typing.Set[str]] = collections.defaultdict(
        lambda: set()
    )
    for line in lines:
        if line.isspace():
            pass
        [machine, compiler, *rest] = line.split()
        machines[machine].add(compiler)
    return machines


def get_machines(
    machine_predicate: typing.Optional[str], compiler_predicate: typing.Optional[str]
) -> typing.Dict[str, typing.Set[str]]:
    predicate_args: typing.Iterable[str] = make_predicate_args(
        machine_predicate, compiler_predicate
    )
    args: typing.List[str] = ["act", "configure", "list-compilers", *predicate_args]
    proc: subprocess.CompletedProcess = subprocess.run(
        args, capture_output=True, text=True
    )
    stdout: str = proc.stdout
    return parse_compiler_list(stdout.splitlines())


@dataclass
class TestInstance:
    """Object representing a single run of the tester, with a given machine, compiler, and file."""

    compiler: str
    subject: str

    def populate_driver(self, driver_template: str) -> str:
        """Populates a driver template with test run data, turning it into a shell command.
        >>> TestInstance(compiler="spam", subject="eggs").populate_driver(
        ...     "./scripts/obj_splitmus {compiler} {subject}")
        './scripts/obj_splitmus spam eggs'

        :param driver_template: A format template into which we substitute test run data.
        :param compiler: The compiler fully-qualified-ID, substituted for 'compiler' in the driver template.
        :param subject: The name of the file under test, substituted for 'subject' in the driver template.
        :return:
        """
        return driver_template.format(compiler=self.compiler, subject=self.subject)

    def run(self, driver_template: str):
        cmd = self.populate_driver(driver_template)
        subprocess.run(cmd, shell=True)


@dataclass
class TestSpec:
    """A specification of how to run a compiler test."""

    machine_predicate: typing.Optional[str]
    compiler_predicate: typing.Optional[str]
    driver: str
    subjects: typing.List[str]

    def run(self):
        machines: typing.Dict[str, typing.Set[str]] = get_machines(
            self.machine_predicate, self.compiler_predicate
        )
        for (machine_id, compilers) in machines.items():
            for compiler_id in compilers:
                fqid = ".".join([machine_id, compiler_id])
                for subject in self.subjects:
                    ti: TestInstance = TestInstance(fqid, subject)
                    ti.run(self.driver)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "-m",
        "--machines",
        metavar="SEXP",
        type=str,
        help="A filtering predicate to use to restrict the machines under test.",
    )
    parser.add_argument(
        "-c",
        "--compilers",
        metavar="SEXP",
        type=str,
        help="A filtering predicate to use to restrict the compilers under test.",
    )
    parser.add_argument(
        "-d",
        "--driver",
        metavar="TEMPLATE",
        type=str,
        help="Template for invoking the test driver",
    )
    parser.add_argument(
        "subjects",
        metavar="FILE",
        type=str,
        nargs="+",
        help="The C/litmus files to test.",
    )

    args = parser.parse_args()
    test = TestSpec(
        compiler_predicate=args.compilers,
        machine_predicate=args.machines,
        driver=args.driver,
        subjects=args.subjects,
    )
    test.run()
