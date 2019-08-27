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
"""Utilities for scraping machine information from ACT."""

import collections
import subprocess
import typing

from act_py import act_id


def get_machines(
    machine_predicate: typing.Optional[str], compiler_predicate: typing.Optional[str]
) -> typing.Dict[act_id.Id, typing.Set[act_id.Id]]:
    """Gets machine information from ACT's configuration, and parses it.

    :param machine_predicate:
        An optional S-expression representing the machine filtering predicate
        to use.
    :param compiler_predicate:
        An optional S-expression representing the compiler filtering predicate
        to use.
    :return:
        A mapping from machine IDs to zero or more compiler IDs.
    """
    predicate_args: typing.Iterable[str] = make_predicate_args(
        machine_predicate, compiler_predicate
    )
    stdout = run_list_compilers(predicate_args)
    return parse_compiler_list(stdout.splitlines())


def run_list_compilers(predicate_args):
    """Runs the ACT list-compilers program with the given arguments.

    :param predicate_args: The arguments to send to the compiler lister.
    :return:
        The standard output of the process if the lister completed
        successfully.
    :raise: `ActMachineInspectError` if the lister failed.
    """
    args: typing.List[str] = ["act", "configure", "list-compilers", *predicate_args]
    proc: subprocess.CompletedProcess = subprocess.run(
        args, capture_output=True, text=True
    )
    try:
        proc.check_returncode()
    except subprocess.CalledProcessError as e:
        raise ActMachineInspectError(proc.stderr) from e
    stdout: str = proc.stdout
    return stdout


class ActMachineInspectError(Exception):
    """Error raised when a call to `get_machines` receives a bad response from
    another ACT program.
    """

    act_message: str

    def __init__(self, act_message: str):
        self.act_message = act_message
        super().__init__(f"Error asking ACT for machine information: {self.act_message}")


def make_predicate_args(
    machine_predicate: typing.Optional[str], compiler_predicate: typing.Optional[str]
) -> typing.Iterator[str]:
    """Yields an argument vector for invoking an act command with the given machine and compiler predicate.

    >>> list(make_predicate_args(None, None))
    []

    >>> list(make_predicate_args('(spam spam sausage)', '(eggs ham)'))
    ['-filter-machines', '(spam spam sausage)', '-filter-compilers', '(eggs ham)']

    :param machine_predicate: An optional predicate to supply to the command to filter machines.
    :param compiler_predicate: An optional predicate to supply to the command to filter compilers.
    :return: An iterator of act command arguments.
    """
    if machine_predicate is not None:
        yield "-filter-machines"
        yield machine_predicate
    if compiler_predicate is not None:
        yield "-filter-compilers"
        yield compiler_predicate


def parse_compiler_list(
    lines: typing.Iterable[str]
) -> typing.Dict[act_id.Id, typing.Set[act_id.Id]]:
    """Parses a compiler information list from `act configure list-compilers`.

    >>> parse_compiler_list([ 'localhost gcc.x86.O0',
    ...                       'localhost gcc.x86.O3',
    ...                       'localhost clang.x86.O3',
    ...                       'farfaraway msvc.x86.O3' ]) == {
    ... 'localhost': {'gcc.x86.O0', 'gcc.x86.O3', 'clang.x86.O3'}, 'farfaraway': {'msvc.x86.O3'}}
    True

    :param lines: Iterable of lines in the compiler list.
    :return: A dictionary mapping machine IDs to sets of compiler IDs.
    """
    machines: typing.DefaultDict[
        act_id.Id, typing.Set[act_id.Id]
    ] = collections.defaultdict(lambda: set())
    for line in lines:
        if line.isspace():
            pass
        [machine, compiler, *_] = line.split()
        machines[act_id.Id(machine)].add(act_id.Id(compiler))
    return machines
