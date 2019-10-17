import json
import pathlib
import typing
from dataclasses import dataclass

from act_py import act_id, json_utils, test_common

@dataclass
class CompilerTest:
    """A grouping of compiler metadata and instances."""
    compiler_id: act_id.Id
    machine_id: act_id.Id
    backend: act_id.Id

    @property
    def qualified_compiler_id(self) -> act_id.Id:
        """Gets this compiler's qualified ID.

        :return: the qualified compiler ID.
        """
        return act_id.qualify(self.machine_id, self.compiler_id)

    def instances(self, env: test_common.Env) -> typing.Iterable[test_common.Instance]:
        """Gets a sequence of test instances for this compiler.

        :param env: The environment from which we get the test subjects.
        :return: An iterable sequence of test instances.
        """
        return (test_common.Instance(self.backend, self.qualified_compiler_id, subject, env) for subject in env.subjects)


@dataclass
class MachineTest:
    """A specification of how to run a multi-compiler test on one machine."""

    compilers: typing.List[act_id.Id]
    backend: act_id.Id

    def compiler_tests(self, machine_id: act_id.Id) -> typing.Iterable[CompilerTest]:
        """Gets a sequence of compiler tests.

         :param machine_id:
            The ID of the machine, used to generate fully qualified compiler
            IDs.
        :return: an iterable sequence of compiler tests; this sequence is
        generated fresh each time this method is called.
        """
        return (CompilerTest(compiler_id, machine_id, self.backend) for compiler_id in self.compilers)


@dataclass
class Test:
    """A specification of how to run a multi-compiler test."""

    env: test_common.Env
    machines: typing.Mapping[act_id.Id, MachineTest]

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
