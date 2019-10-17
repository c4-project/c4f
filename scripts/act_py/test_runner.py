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
import logging
import multiprocessing
import subprocess
from dataclasses import dataclass

from act_py import act_id, test_common, test

logger = logging.getLogger(__name__)


@dataclass
class Runner:
    """Controls test runs."""

    def run(self, test: test.Test) -> None:
        """Reports on the given test.

        :param test: The test to summarise.
        :return: Nothing.
        """
        test.env.prepare()
        for machine_id, machine in test.machines.items():
            self.run_on_machine(machine_id, machine, test.env)

    def run_on_machine(self, machine_id: act_id.Id, machine: test.MachineTest, env: test_common.Env):
        """Reports on the given machine test.

        :param machine_id: The ID of the machine to summarise.
        :param machine: The machine test to summarise.
        :param env: The test's environment block.
        :return: Nothing.
        """
        for compiler in machine.compiler_tests(machine_id):
            self.run_on_compiler(compiler, env)

    def run_on_compiler(self, compiler: test.CompilerTest, env: test_common.Env):
        """Reports on the given compiler test.
        :param compiler: The compiler test to summarise.
        :param env: The test's environment block.
        :return: Nothing.
        """
        for instance in compiler.instances(env):
            self.run_on_instance(instance)

    def run_on_instance(self, instance: test_common.Instance) -> None:
        """Runs the given test instance.

        :param instance: The instance for which we are running a test.
        :return: Nothing.
        """
        instance.prepare()

        logger.info("%s: running driver", str(self))
        proc: subprocess.CompletedProcess = subprocess.run(
            instance.driver_command, shell=True, text=True, capture_output=True
        )
        instance.log_result(logger, proc)
        instance.write_files(proc)
