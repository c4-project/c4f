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

from act_py import observation, test_common

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


class MarkdownReporter:
    """Object that handles reports by emitting them as markdown."""

    io: typing.TextIO

    def __init__(self, file: typing.TextIO):
        self.file = file

    def print_observation_header(
        self, name: str, flags: typing.Set[observation.Flag], has_errors: bool
    ):
        flag_strs = [f.value for f in flags] + (["has errors"] if has_errors else [])
        flag_str = ", ".join(flag_strs)

        print("", file=self.file)
        print(f"## {name} ({flag_str})", file=self.file)

    def print_states_with_header(
        self, header: str, states: typing.List[typing.Mapping[str, str]]
    ) -> None:
        print()
        print(f"### {header} ({len(states)}):")
        print()
        self.print_states(states)

    def print_states(self, states: typing.List[typing.Mapping[str, str]]) -> None:
        for state in states:
            self.print_state(state)
        # This is done because the checker occasionally sends stderr output soon
        # after using stdout, and it can race with the state output.
        sys.stdout.flush()

    def print_state(self, cx: typing.Mapping[str, str]) -> None:
        print("  -", end=" ", file=self.file)
        json.dump(cx, self.file)
        print(file=self.file)


class ReportPhase(test_common.Phase):
    """Object representing a single run of the reporter, with a given machine, compiler, and subject."""

    settings: ReportSettings

    def __init__(
        self,
        reporter: MarkdownReporter,
        settings: ReportSettings,
        instance: test_common.Instance,
    ):
        test_common.Phase.__init__(self, instance)
        self.reporter = reporter
        self.settings = settings

    def run(self) -> None:
        """Runs this test instance."""
        obs = observation.load_from_path(self.state_file)
        self.inspect_observation(obs)

    def inspect_observation(self, obs: observation.Observation) -> None:
        """Inspects the given observation, grumbling onto stdout about what
        lies within.

        :param obs: The observation to inspect.
        """
        if not self.is_printable(obs):
            return
        self.reporter.print_observation_header(
            self.instance.name, obs.flags, self.has_errors
        )
        self.print_state_breakdown(obs)

    def is_printable(self, obs: observation.Observation) -> bool:
        if obs.is_missing:
            return True
        if obs.is_sat and self.settings.print_sat:
            return True
        if obs.is_unsat and self.settings.print_unsat:
            return True
        if self.settings.print_unknown:
            return True
        return False

    @property
    def has_errors(self) -> bool:
        """Checks whether this subject encountered errors during its last run.

        :return: True if an error file exists and is nonempty; false otherwise.
        """
        try:
            with open(self.error_file, "r") as f:
                lines = f.readlines()
                return lines != []
        except FileNotFoundError:
            return False

    def print_state_breakdown(self, obs: observation.Observation) -> None:
        if self.settings.print_witnesses:
            self.reporter.print_states_with_header("Witnesses", obs.witnesses)
        if self.settings.print_counter_examples:
            self.reporter.print_states_with_header(
                "Counter-examples", obs.counter_examples
            )


def report_phase(
    settings: ReportSettings, instance: test_common.Instance
) -> test_common.Phase:
    """Function wrapper for `ReportPhase`.

    :param settings: The settings to use for the check.
    :param instance: The instance to use for the check.
    :return: A `test_common.Phase` that, when run, checks the test result for
        `instance`.
    """
    return ReportPhase(MarkdownReporter(sys.stdout), settings, instance)
