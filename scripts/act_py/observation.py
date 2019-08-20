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
"""Classes for working with observations."""
import json

import enum
import typing
from dataclasses import dataclass


class Flag(enum.Enum):
    """Flags used in observations."""

    SAT = "sat"
    UNSAT = "unsat"
    UNDEF = "undef"


@dataclass
class Observation:
    """A state observation.

    State observations contain various breakdowns of the states observed, as
    well as information about whether the state satisfied some expected
    (and implicit) postcondition, did not satisfy the postcondition,
    or was ill-defined.
    """

    counter_examples: typing.List[typing.Mapping[str, str]]

    witnesses: typing.List[typing.Mapping[str, str]]

    states: typing.List[typing.Mapping[str, str]]

    flags: typing.Set[Flag]

    @property
    def is_sat(self) -> bool:
        """Checks whether this observation satisfied its postcondition.

        Examples:

        >>> Observation([], [], [], set()).is_sat
        False

        >>> Observation([], [], [], {Flag.UNSAT}).is_sat
        False

        >>> Observation([], [], [], {Flag.SAT}).is_sat
        True

        :return: True if the `Flag.SAT` flag is enabled on this observation;
        false otherwise.
        """
        return Flag.SAT in self.flags

    @property
    def is_unsat(self) -> bool:
        """Checks whether this observation expressly did not satisfy its postcondition.

        Examples:

        >>> Observation([], [], [], set()).is_unsat
        False

        >>> Observation([], [], [], {Flag.SAT}).is_unsat
        False

        >>> Observation([], [], [], {Flag.UNSAT}).is_unsat
        True

        :return: True if the `Flag.UNSAT` flag is enabled on this observation;
        false otherwise.
        """
        return Flag.UNSAT in self.flags


def load(fp: typing.TextIO) -> Observation:
    """Loads an observation from a file pointer.

    :param fp: The (text) file pointer from which we will load an observation.
    :return: The resulting `Observation` object.
    """
    obs_dict: typing.Dict[str, typing.Any] = json.load(fp)
    return of_dict(obs_dict)


def of_dict(obs_dict: typing.Dict[str, typing.Any]) -> Observation:
    """Loads an observation from a dictionary.

    :param obs_dict: The dictionary from which we are loading the observation.
    :return: The resulting `Observation` object.
    """
    flags: typing.Set[Flag] = {Flag(s) for s in obs_dict["flags"]}
    states = ensure_state_dicts(obs_dict["states"])
    witnesses = ensure_state_dicts(obs_dict["witnesses"])
    counter_examples = ensure_state_dicts(obs_dict["counter_examples"])
    return Observation(counter_examples, witnesses, states, flags)


def ensure_state_dicts(candidate: typing.Any) -> typing.List[typing.Mapping[str, str]]:
    """Makes sure an incoming state dictionary-list has the right type.

    :param candidate: The state dictionary to check.
    :return: The validated `typing.Mapping`.
    """
    return [{str(k): str(v) for k, v in x.items()} for x in candidate]
