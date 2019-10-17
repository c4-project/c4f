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
"""The main part of the ACT test results checker."""
import json
import sys
import logging
from dataclasses import dataclass

import typing

from act_py import act_id, observation, test, test_common

logger = logging.getLogger(__name__)


@dataclass
class ReportSettings:
    """Settings for the reporter."""

    # Whether the reporter should announce satisfiability results.
    print_sat: bool = False

    # Whether the reporter should announce unsatisfiability results.
    print_unsat: bool = True

    # Whether the reporter should announce unknown-satisfiability results.
    print_unknown: bool = True

    # Whether the reporter should print out all witnesses.
    print_witnesses: bool = False

    # Whether the reporter should print out all counter-examples.
    print_counter_examples: bool = True


class MarkdownReportWriter:
    """Object that handles reports by emitting them as markdown."""

    io: typing.TextIO

    def __init__(self, file: typing.TextIO):
        self.file = file

    def print_header(self, level: int, body: str) -> None:
        """Prints a markdown header.

        :param level: The level of the header (minimum 1).
        :param body: The body of the header.
        :return: Nothing.
        """
        print('#' * max(1, level), body, file=self.file)
        print(file=self.file)

    def print_machine_header(self, name: str) -> None:
        """Prints a header for the given machine.

        :param name: The name of the machine.
        :return: Nothing.
        """
        self.print_header(1, f"Machine {name}")

    def print_compiler_header(self, name: str) -> None:
        """Prints a header for the given compiler.

        :param name: The name of the compiler.
        :return: Nothing.
        """
        self.print_header(2, f"Compiler {name}")

    def print_observation_header(
        self, name: str, flags: typing.Set[observation.Flag], has_errors: bool
    ) -> None:
        flag_strs = [f.value for f in flags] + (["has errors"] if has_errors else [])
        flag_str = ", ".join(flag_strs)

        self.print_header(3, f"{name} ({flag_str})")

    def print_states_with_header(
        self, header: str, states: typing.List[typing.Mapping[str, str]]
    ) -> None:
        if not states:
            print(f"No {header.lower()}.", file=self.file)
            print(file=self.file)
            return
        self.print_header(4, f"{header} ({len(states)}):")
        self.print_states(states)
        print(file=self.file)

    def print_states(self, states: typing.List[typing.Mapping[str, str]]) -> None:
        for state in states:
            self.print_state(state)
        # This is done because the checker occasionally sends stderr output soon
        # after using stdout, and it can race with the state output.
        self.file.flush()

    def print_state(self, cx: typing.Mapping[str, str]) -> None:
        print("  -", end=" ", file=self.file)
        json.dump(cx, self.file)
        print(file=self.file)


class Reporter:
    """Object representing a single run of the reporter, with a given machine, compiler, and subject."""

    settings: ReportSettings

    def __init__(
        self,
        reporter: MarkdownReportWriter,
        settings: ReportSettings,
    ):
        self.reporter = reporter
        self.settings = settings

    def run(self, test: test.Test) -> None:
        """Reports on the given test.

        :param test: The test to summarise.
        :return: Nothing.
        """
        for machine_id, machine in test.machines.items():
            self.run_on_machine(machine_id, machine, test.env)

    def run_on_machine(self, machine_id: act_id.Id, machine: test.MachineTest, env: test_common.Env):
        """Reports on the given machine test.

        :param machine_id: The ID of the machine to summarise.
        :param machine: The machine test to summarise.
        :param env: The test's environment block.
        :return: Nothing.
        """
        self.reporter.print_machine_header(machine_id)
        for compiler in machine.compiler_tests(machine_id):
            self.run_on_compiler(compiler, env)

    def run_on_compiler(self, compiler: test.CompilerTest, env: test_common.Env):
        """Reports on the given compiler test.
        :param compiler: The compiler test to summarise.
        :param env: The test's environment block.
        :return: Nothing.
        """
        self.reporter.print_compiler_header(compiler.compiler_id)
        for instance in compiler.instances(env):
            self.run_on_instance(instance)

    def run_on_instance(self, instance: test_common.Instance) -> None:
        """Reports over a test instance."""
        obs = observation.load_from_path(instance.state_file)
        self.inspect_observation(obs, instance)

    def inspect_observation(self, obs: observation.Observation, instance: test_common.Instance) -> None:
        """Inspects the given observation, grumbling onto stdout about what
        lies within.

        :param obs: The observation to inspect.
        :param instance: The observed test instance.
        """
        if not self.is_printable(obs):
            return
        self.reporter.print_observation_header(
            instance.name, obs.flags, instance.has_errors
        )
        self.print_state_breakdown(obs)

    def is_printable(self, obs: observation.Observation) -> bool:
        if obs.is_missing:
            return True
        if obs.is_sat:
            return self.settings.print_sat
        if obs.is_unsat:
            return self.settings.print_unsat
        if self.settings.print_unknown:
            return True
        return False


    def print_state_breakdown(self, obs: observation.Observation) -> None:
        if self.settings.print_witnesses:
            self.reporter.print_states_with_header("Witnesses", obs.witnesses)
        if self.settings.print_counter_examples:
            self.reporter.print_states_with_header(
                "Counter-examples", obs.counter_examples
            )
