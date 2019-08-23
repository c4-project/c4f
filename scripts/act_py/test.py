import functools
import json
import pathlib
import typing
from dataclasses import dataclass

from act_py import act_id, json_utils, test_reporter, test_common, test_runner

PhaseFactory = typing.Callable[[test_common.Instance], test_common.Phase]


@dataclass
class MachineTest:
    """A specification of how to run a multi-compiler test on one machine."""

    compilers: typing.List[act_id.Id]
    backend: act_id.Id

    def run_phase(
        self, machine_id: act_id.Id, env: test_common.Env, phase_factory: PhaseFactory
    ) -> None:
        """Runs a tester phase for all compilers on this machine.

        :param machine_id:
            The ID of the machine, used to generate fully qualified compiler
            IDs.
        :param env:
            The `Env` containing test subjects and drivers.
        :param phase_factory:
            A function that creates the testing phase required.
        """
        for compiler_id in self.compilers:
            q_compiler_id = act_id.qualify(machine_id, compiler_id)
            self.run_phase_on_compiler(q_compiler_id, env, phase_factory)

    def run_phase_on_compiler(
        self,
        q_compiler_id: act_id.Id,
        env: test_common.Env,
        phase_factory: PhaseFactory,
    ) -> None:
        """Runs tests for a single compiler on this machine.

        :param q_compiler_id:
            The fully qualified ID of the compiler.
        :param env:
            The `Env` containing test subjects and drivers.
        :param phase_factory:
            The type of phase to run.  This type is used as a factory.
        """
        for subject in env.subjects:
            instance = test_common.Instance(self.backend, q_compiler_id, subject, env)
            ti: test_common.Phase = phase_factory(instance)
            ti.run()


@dataclass
class Test:
    """A specification of how to run a multi-compiler test."""

    env: test_common.Env
    machines: typing.Mapping[act_id.Id, MachineTest]

    def do_phase(self, phase_factory: PhaseFactory) -> None:
        for (machine_id, machine_spec) in self.machines.items():
            machine_spec.run_phase(machine_id, self.env, phase_factory)

    def run(self) -> None:
        """Runs the test specified by this object."""
        self.env.prepare()
        self.do_phase(test_runner.run_phase)

    def check(self, settings: test_reporter.ReportSettings) -> None:
        """Checks the results of the test specified by this object.

        :param settings: The settings to use for the checker.
        """
        print(f"# Test summary")
        self.do_phase(functools.partial(test_reporter.check_phase, settings))

    def dump(self, fp: typing.TextIO) -> None:
        """Dumps this test, as JSON, to the given file pointer.

        :param fp: The file to which we are dumping this test.
        """
        json.dump(self, fp, sort_keys=True, indent=4, cls=json_utils.ExtendedEncoder)


def machine_test_from_dict(d: typing.Mapping[str, typing.Any]) -> MachineTest:
    compilers = [act_id.Id(compiler) for compiler in d["compilers"]]
    backend = act_id.Id(d["backend"])
    return MachineTest(compilers=compilers, backend=backend)


def test_subject_from_dict(d: typing.Mapping[str, typing.Any]) -> test_common.Subject:
    name = str(d["name"])
    path = pathlib.Path(d["path"])
    return test_common.Subject(name=name, path=path)


def test_env_from_dict(d: typing.Mapping[str, typing.Any]) -> test_common.Env:
    subjects = [test_subject_from_dict(subject) for subject in d["subjects"]]
    driver = str(d["driver"])
    output_dir = pathlib.Path(d["output_dir"])
    return test_common.Env(subjects=subjects, driver=driver, output_dir=output_dir)


def of_dict(d: typing.Mapping[str, typing.Any]) -> Test:
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


def load(fp: typing.TextIO) -> Test:
    """Loads a test file from a file pointer.

    :param fp: The (text) file pointer from which we will load a test file.
    :return: The resulting `Test` object.
    """
    aux_dict: typing.Dict[str, typing.Any] = json.load(fp)
    return of_dict(aux_dict)
