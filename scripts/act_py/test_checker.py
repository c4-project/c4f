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

from act_py import act_id, io_utils, observation, test_common

logger = logging.getLogger(__name__)


@dataclass
class CheckSettings:
    """Settings for the checker."""

    # Whether the checker should announce satisfiability results.
    print_sat: bool = False

    # Whether the checker should announce unsatisfiability results.
    print_unsat: bool = True

    # Whether the checker should announce unknown-satisfiability results.
    print_unknown: bool = True

    # Whether the checker should print out all witnesses.
    print_witnesses: bool = False

    # Whether the checker should print out all counter-examples.
    print_counter_examples: bool = True


class CheckPhase(test_common.Phase):
    """Object representing a single run of the checker, with a given machine, compiler, and subject."""

    settings: CheckSettings

    def __init__(self, settings: CheckSettings, instance: test_common.Instance):
        test_common.Phase.__init__(self, instance)
        self.settings = settings

    def run(self) -> None:
        """Runs this test instance."""
        # Errors in the error file might not actually mean that the test failed.
        # They could just be compiler warnings.
        self.report_errors()
        self.inspect_observation_file()

    def report_errors(self) -> None:
        """Complains to stderr if the test has errors."""
        if self.has_errors:
            print(
                f"{self.instance.name} ERROR (see {self.error_file}).", file=sys.stderr
            )

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

    def inspect_observation_file(self) -> None:
        """Opens the observation file for this test instance, and grumbles onto
        stdout about what lies within.
        """
        try:
            with open(self.state_file, "r") as fp:
                obs = observation.load(fp)
        except FileNotFoundError:
            return
        self.inspect_observation(obs)

    def inspect_observation(self, obs: observation.Observation) -> None:
        """Inspects the given observation, grumbling onto stdout about what
        lies within.

        :param obs: The observation to inspect.
        """
        if obs.is_sat:
            self.report_sat(obs)
        elif obs.is_unsat:
            self.report_unsat(obs)
        else:
            self.report_unknown(obs)

    def report_sat(self, obs: observation.Observation) -> None:
        """Reports that `obs` satisfied its postcondition, pursuant to the
        checker's reporting settings.

        :param obs: The observation to inspect.
        """
        if not self.settings.print_sat:
            return
        print(f"{self.instance.name} SAT.")
        self.print_state_breakdown(obs)

    def report_unsat(self, obs: observation.Observation) -> None:
        """Reports that `obs` did not satisfy its postcondition, pursuant to the
        checker's reporting settings.

        :param obs: The observation to inspect.
        """
        if not self.settings.print_unsat:
            return
        print(f"{self.instance.name} UNSAT.")
        self.print_state_breakdown(obs)

    def report_unknown(self, obs: observation.Observation) -> None:
        """Reports that `obs` has an unknown satisfiability, pursuant to the
        checker's reporting settings.

        :param obs: The observation to inspect.
        """
        if not self.settings.print_unknown:
            return
        print(f"{self.instance.name} UNKNOWN STATUS.")
        self.print_state_breakdown(obs)

    def print_state_breakdown(self, obs: observation.Observation):
        if self.settings.print_witnesses:
            print(f"  Witnesses ({len(obs.witnesses)}):")
            print_states(obs.witnesses)
        if self.settings.print_counter_examples:
            print(f"  Counter-examples ({len(obs.counter_examples)}):")
            print_states(obs.counter_examples)


def print_states(states: typing.List[typing.Mapping[str, str]]) -> None:
    for state in states:
        print_state(state)
    # This is done because the checker occasionally sends stderr output soon
    # after using stdout, and it can race with the state output.
    sys.stdout.flush()


def print_state(cx: typing.Mapping[str, str]) -> None:
    print("    -", end=" ")
    json.dump(cx, sys.stdout)
    print("")


def check_phase(
    settings: CheckSettings, instance: test_common.Instance
) -> test_common.Phase:
    """Function wrapper for `CheckPhase`.

    :param settings: The settings to use for the check.
    :param instance: The instance to use for the check.
    :return: A `test_common.Phase` that, when run, checks the test result for
        `instance`.
    """
    return CheckPhase(settings, instance)
