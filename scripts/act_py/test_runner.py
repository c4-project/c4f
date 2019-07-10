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
"""The main part of the ACT high-level test runner."""
import json
from dataclasses import dataclass
import logging
import pathlib
import subprocess
import typing

from act_py import act_id, io_utils, json_utils

logger = logging.getLogger(__name__)


@dataclass
class TestSubject:
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
class TestEnv:
    """The part of a test specification that doesn't change between machines."""

    subjects: typing.List[TestSubject]
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

    def output_dir_for(self, subject: TestSubject, compiler: act_id.Id) -> pathlib.Path:
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
class TestInstance:
    """Object representing a single run of the tester, with a given machine, compiler, and subject."""

    backend: act_id.Id
    compiler: act_id.Id
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
            "dir": self.output_dir,
            **self.subject.driver_dict,
        }

    def run(self) -> None:
        """Runs this test instance."""
        self.output_dir.mkdir(exist_ok=True)

        logger.info("%s: running driver", str(self))
        proc: subprocess.CompletedProcess = subprocess.run(
            self.driver_command, shell=True, text=True, capture_output=True
        )
        self.log_result(proc)
        self.write_files(proc)

    def log_result(self, proc: subprocess.CompletedProcess) -> None:
        """Logs the exit code of a driver.

        :param proc: The completed process corresponding to the driver.
        """
        if proc.returncode == 0:
            logger.info("%s: success", str(self))
        else:
            logger.error("%s: failed: see %s for details", str(self), self.error_file)

    def write_files(self, proc: subprocess.CompletedProcess) -> None:
        """Writes the output of a driver to specific files in the output
        directory.

        :param proc: The completed process corresponding to the driver.
        """
        io_utils.write_text(self.error_file, proc.stderr)
        if proc.returncode == 0:
            io_utils.write_text(self.state_file, proc.stdout)

    @property
    def driver_command(self) -> str:
        """Gets the fully populated driver shell command for this instance.
        :return: The populated driver template.
        """
        return self.env.populate_driver(**self.driver_dict)

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

    def __str__(self):
        return f"{self.compiler}!{self.subject.name}"


@dataclass
class MachineTest:
    """A specification of how to run a multi-compiler test on one machine."""

    compilers: typing.List[act_id.Id]
    backend: act_id.Id

    def run(self, machine_id: act_id.Id, env: TestEnv) -> None:
        """Runs tests for all compilers on this machine.

        :param machine_id:
            The ID of the machine, used to generate fully qualified compiler
            IDs.
        :param env:
            The `TestEnv` containing test subjects and drivers.
        """
        for compiler_id in self.compilers:
            q_compiler_id = act_id.qualify(machine_id, compiler_id)
            self.run_compiler(q_compiler_id, env)

    def run_compiler(self, q_compiler_id: act_id.Id, env: TestEnv) -> None:
        """Runs tests for a single compiler on this machine.

        :param q_compiler_id:
            The fully qualified ID of the compiler.
        :param env:
            The `TestEnv` containing test subjects and drivers.
        """
        for subject in env.subjects:
            ti: TestInstance = TestInstance(
                backend=self.backend, compiler=q_compiler_id, subject=subject, env=env
            )
            ti.run()


@dataclass
class Test:
    """A specification of how to run a multi-compiler test."""

    env: TestEnv
    machines: typing.Mapping[act_id.Id, MachineTest]

    def run(self) -> None:
        """Runs the test specified by this object."""
        for (machine_id, machine_spec) in self.machines.items():
            machine_spec.run(machine_id, self.env)

    def dump(self, fp: typing.TextIO) -> None:
        """Dumps this test, as JSON, to the given file pointer.

        :param fp: The file to which we are dumping this test.
        """
        json.dump(self, fp, sort_keys=True, indent=4, cls=json_utils.ExtendedEncoder)


def machine_test_from_dict(d: typing.Mapping[str, typing.Any]) -> MachineTest:
    compilers = [act_id.Id(compiler) for compiler in d["compilers"]]
    backend = act_id.Id(d["backend"])
    return MachineTest(compilers=compilers, backend=backend)


def test_subject_from_dict(d: typing.Mapping[str, typing.Any]) -> TestSubject:
    name = str(d["name"])
    path = pathlib.Path(d["path"])
    return TestSubject(name=name, path=path)


def test_env_from_dict(d: typing.Mapping[str, typing.Any]) -> TestEnv:
    subjects = [test_subject_from_dict(subject) for subject in d["subjects"]]
    driver = str(d["driver"])
    output_dir = pathlib.Path(d["output_dir"])
    return TestEnv(subjects=subjects, driver=driver, output_dir=output_dir)


def test_from_dict(d: typing.Mapping[str, typing.Any]) -> Test:
    """
    Constructs a `Test` from a given `dict` (such as the result of reading in
    a JSON test spec).

    :param d: The dictionary to try to convert to a `Test`.
    :return: The `Test` whose fields correspond to the data in `d`.
    """
    machines = {
        act_id.Id(mid): machine_test_from_dict(md) for mid, md in d["machines"].items()
    }
    env = test_env_from_dict(d["env"])
    return Test(machines=machines, env=env)
