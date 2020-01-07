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
import os
import subprocess
import typing

from act_py import act_id


# TODO(@MattWindsor91): this is horrific and needs to be done better.
SCRIPT_DIR: os.PathLike = os.path.join(os.path.dirname(__file__), os.path.pardir)


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
    args: typing.List[str] = ["act-compiler", "list", *predicate_args]
    proc: subprocess.CompletedProcess = subprocess.run(
        args, capture_output=True, text=True
    )
    try:
        proc.check_returncode()
    except subprocess.CalledProcessError as e:
        raise ActMachineInspectError(proc.stderr) from e
    stdout: str = proc.stdout
    return stdout


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
    """Parses a compiler information list from `act-c list-compilers`.

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


class ActInspectError(Exception):
    """Error raised when a call into `ACT` gets a bad response."""

    act_message: str

    def __init__(self, type_of_information: str, act_message: str):
        self.act_message = act_message
        super().__init__(
            f"Error asking ACT for {type_of_information} information: {self.act_message}"
        )


def get_backend_for(
    machine_id: typing.Optional[act_id.Id], style_id: typing.Optional[act_id.Id]
):
    """Runs the 'get_a_backend' script with the given arguments.

    :param machine_id: The (optional) machine ID to request.
    :param style_id: The (optional) style ID to request.
    :return:
        The standard output of the process if the lister completed
        successfully.
    :raise: `ActMachineInspectError` if the lister failed.
    """
    cmd = str(os.path.join(SCRIPT_DIR, "get_a_backend"))

    machine_args = [] if machine_id is None else ["-m", str(machine_id)]
    style_args = [] if style_id is None else ["-s", str(style_id)]
    args = [cmd, *machine_args, *style_args]

    proc: subprocess.CompletedProcess = subprocess.run(
        args, capture_output=True, text=True
    )
    try:
        proc.check_returncode()
    except subprocess.CalledProcessError as e:
        raise ActBackendInspectError(proc.stderr) from e
    stdout: str = proc.stdout
    return stdout


class ActMachineInspectError(ActInspectError):
    """Error raised when a call to `get_machines` receives a bad response from
    another ACT program.
    """

    def __init__(self, act_message: str):
        super().__init__("machine", act_message)


class ActBackendInspectError(ActInspectError):
    """Error raised when a call to `get_backend_for_machine` receives a bad response from
    another ACT program.
    """

    def __init__(self, act_message: str):
        super().__init__("machine backend", act_message)
