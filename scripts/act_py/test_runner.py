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

from dataclasses import dataclass
import typing
import os
import subprocess

from act_py import io_utils


@dataclass
class TestSubject:
    """Contains information about a single test subject."""

    name: str
    path: str

    @property
    def driver_dict(self) -> typing.Mapping[str, typing.Any]:
        """Returns the dictionary of values used to populate driver templates.

        :return: The dictionary used for driver templates.
        """
        return {"subject_name": self.name, "subject_path": self.path}


@dataclass
class TestEnv:
    """The part of a test specification that doesn't change between machines."""

    subjects: typing.Mapping[str, str]
    driver: str
    output_dir: str

    def prepare(self) -> None:
        """Prepares the environment by making the output directory, etc."""
        self.make_output_dir()
        self.prepare_subjects()

    def make_output_dir(self) -> None:
        """Tries to make the output directory if it doesn't yet exist."""
        io_utils.try_mkdir(self.output_dir)

    def prepare_subjects(self) -> None:
        """Prepares the environment for each each test subject."""
        for path in self.subjects.values():
            io_utils.check_file_exists(path)
            io_utils.try_mkdir(path)

    def output_dir_for(self, subject: TestSubject) -> str:
        """Gets the appropriate subdirectory of this environment's output directory
        for a specific subject.

        This method doesn't create the directory itself.

        :param subject: The subject for which we need an output directory.
        :return: The path to the output directory.
        """
        return os.path.join(self.output_dir, subject.name)

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
class TestInstance:
    """Object representing a single run of the tester, with a given machine, compiler, and subject."""

    backend: str
    compiler: str
    subject: TestSubject
    env: TestEnv

    @property
    def driver_dict(self) -> typing.Mapping[str, typing.Any]:
        """Returns the dictionary of values used to populate driver templates.

        This doesn't include any of the environment's values, as they're
        auto-populated when we ask for the populated driver.

        :return: The dictionary used for driver templates.
        """
        return {
            "backend": self.backend,
            "compiler": self.compiler,
            "dir": self.env.output_dir_for(self.subject),
            **self.subject.driver_dict,
        }

    @property
    def driver_command(self) -> str:
        """Gets the fully populated driver shell command for this instance.
        :return: The populated driver template.
        """
        return self.env.populate_driver(**self.driver_dict)

    def run(self):
        subprocess.run(self.driver_command, shell=True)


def qualify_id(machine_id: str, compiler_id: str) -> str:
    """Fully-qualifies a compiler ID by appending it to a machine ID.

    Examples:

    >>> qualify_id("localhost", "gcc.x86.O3")
    "localhost.gcc.x86.O3"

    >>> qualify_id("", "gcc.x86.O3")
    "gcc.x86.O3"
    """
    return compiler_id if machine_id.isspace() else ".".join([machine_id, compiler_id])


@dataclass
class MachineTest:
    compilers: typing.List[str]
    backend: str

    def run(self, machine_id: str, env: TestEnv):
        for compiler_id in self.compilers:
            q_compiler_id = qualify_id(machine_id, compiler_id)
            for (subject_name, subject_path) in env.subjects.items():
                subject: TestSubject = TestSubject(name=subject_name, path=subject_path)
                ti: TestInstance = TestInstance(
                    backend=self.backend,
                    compiler=q_compiler_id,
                    subject=subject,
                    env=env,
                )
                ti.run()


@dataclass
class Test:
    """A specification of how to run a compiler test."""

    env: TestEnv
    machines: typing.Mapping[str, MachineTest]

    def run(self):
        """
        Runs the test specified by this object.
        :return:
        """
        for (machine_id, machine_spec) in self.machines.items():
            machine_spec.run(machine_id, self.env)


def machine_test_from_dict(d: typing.Mapping[str, typing.Any]) -> MachineTest:
    compilers = [str(compiler) for compiler in d["compilers"]]
    backend = str(d["backend"])
    return MachineTest(compilers=compilers, backend=backend)


def test_env_from_dict(d: typing.Mapping[str, typing.Any]) -> TestEnv:
    subjects = {str(name): str(subject) for name, subject in d["subjects"].items()}
    driver = str(d["driver"])
    output_dir = str(d["output_dir"])
    return TestEnv(subjects=subjects, driver=driver, output_dir=output_dir)


def test_from_dict(d: typing.Mapping[str, typing.Any]) -> Test:
    """
    Constructs a `Test` from a given `dict` (such as the result of reading in
    a JSON test spec).

    :param d: The dictionary to try to convert to a `Test`.
    :return: The `Test` whose fields correspond to the data in `d`.
    """
    machines = {
        str(mid): machine_test_from_dict(md) for mid, md in d["machines"].items()
    }
    env = test_env_from_dict(d["env"])
    return Test(machines=machines, env=env)
