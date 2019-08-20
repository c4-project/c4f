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
"""Classes shared between the high-level test specifications and the specific
test-manipulating drivers."""
import abc
import pathlib
import typing
from dataclasses import dataclass

from act_py import act_id, io_utils


@dataclass
class Subject:
    """Contains information about a single test subject."""

    name: str
    path: pathlib.Path

    @property
    def driver_dict(self) -> typing.Mapping[str, typing.Any]:
        """Returns the dictionary of values used to populate driver templates.

        :return: The dictionary used for driver templates.
        """
        return {"subject_name": self.name, "subject_path": self.path}

    def prepare(self) -> None:
        """Prepares the environment for this subject."""
        io_utils.check_file_exists(self.path)


@dataclass
class Env:
    """The part of a test specification that doesn't change between machines."""

    subjects: typing.List[Subject]
    driver: str
    output_dir: pathlib.Path

    def prepare(self) -> None:
        """Prepares the environment by making the output directory, etc."""
        self.make_output_dir()
        self.prepare_subjects()

    def make_output_dir(self) -> None:
        """Tries to make the output directory if it doesn't yet exist."""
        self.output_dir.mkdir(parents=True, exist_ok=True)

    def prepare_subjects(self) -> None:
        """Prepares the environment for each each test subject."""
        for subject in self.subjects:
            subject.prepare()

    def output_dir_for(self, subject: Subject, compiler: act_id.Id) -> pathlib.Path:
        """Gets the appropriate subdirectory of this environment's output directory
        for a specific subject and compiler.

        This method doesn't create the directory itself.

        :param subject: The subject for which we need an output directory.
        :param compiler: The compiler for which we need an output directory.
        :return: The path to the output directory.
        """
        name: str = "_".join([subject.name, act_id.to_dir(compiler)])
        return self.output_dir / name

    def populate_driver(self, **data: str) -> str:
        """Populates this environment's designated driver template with test run data,
        turning it into a shell command.

        The usual keys are:

        backend
            The fully qualified ID of the simulator, runner, or other backend
            that the driver is expected to use to
        compiler
            The fully qualified ID of the compiler being tested.
        subject_name
            The Litmus test name of the file being tested.
        subject_path
            The path to the file being tested (at time of writing, this is
            always a C litmus test).
        dir
            The path to the directory under which auxiliary data for this
            subject may be written (we assume that another part of this
            test runner has created the directory before calling the driver).

        :param data: The data mapping to use for populating the keys above.
        :return: The populated driver command.
        """
        return self.driver.format(**data)


@dataclass
class Instance:
    """A single instance of the tester (combining a backend, compiler,
    subject, and environment)."""

    backend: act_id.Id
    compiler: act_id.Id
    subject: Subject
    env: Env

    @property
    def name(self):
        """Gets a semi-human-readable name for this instance's combination of
        compiler and subject.

        Examples:

        >>> Instance(backend=act_id.Id("foo.bar"),
        ...          compiler=act_id.Id("bar.baz"),
        ...          subject=Subject(name="bloop", path=pathlib.Path(".")),
        ...          env=Env(subjects=[], driver="", output_dir=pathlib.Path("."))).name
        'bar.baz!bloop'

        :return: A name for this instance.
        """
        return f"{self.compiler}!{self.subject.name}"

    @property
    def error_file(self) -> pathlib.Path:
        """Gets the path to the file to which any detailed errors reported for this instance will be written.

        :return: The error file path.
        """
        return self.output_dir / "errors"

    @property
    def state_file(self) -> pathlib.Path:
        """Gets the path to the file to which any final state information for this instance will be written.

        :return: The state file path.
        """
        return self.output_dir / "state.json"

    @property
    def output_dir(self) -> pathlib.Path:
        """Gets the path to the directory to which any intermediate and final output for this instance will be written.

        :return: The output directory path.
        """
        return self.env.output_dir_for(self.subject, self.compiler)


class Phase(abc.ABC):
    """Abstract base class for things that perform a stateful action on
    tests (running them, checking them, etc.).
    """

    instance: Instance

    def __init__(self, instance: Instance):
        self.instance = instance

    @property
    def backend(self) -> act_id.Id:
        """Gets the backend of this phase's instance.

        :return: The backend.
        """
        return self.instance.backend

    @property
    def compiler(self) -> act_id.Id:
        """Gets the compiler of this phase's instance.

        :return: The compiler.
        """
        return self.instance.compiler

    @property
    def subject(self) -> Subject:
        """Gets the subject of this phase's instance.

        :return: The subject.
        """
        return self.instance.subject

    @property
    def env(self) -> Env:
        """Gets the test environment of this phase's instance.

        :return: The test environment.
        """
        return self.instance.env

    @property
    def error_file(self) -> pathlib.Path:
        """Gets the path to the file to which any detailed errors reported for this instance will be written.

        :return: The error file path.
        """
        return self.instance.error_file

    @property
    def state_file(self) -> pathlib.Path:
        """Gets the path to the file to which any final state information for this instance will be written.

        :return: The state file path.
        """
        return self.instance.state_file

    @property
    def output_dir(self) -> pathlib.Path:
        """Gets the path to the directory to which any intermediate and final output for this instance will be written.

        :return: The output directory path.
        """
        return self.instance.output_dir

    @abc.abstractmethod
    def run(self) -> None:
        """Runs this test instance."""
        pass

    def __str__(self):
        return self.instance.name
