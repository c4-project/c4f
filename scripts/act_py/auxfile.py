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

import pathlib
import typing
import json
from dataclasses import dataclass

from . import litmus_id


@dataclass
class LitmusAux:
    locations: typing.Optional[typing.List[str]]
    init: typing.Optional[typing.Dict[str,int]]
    postcondition: typing.Optional[str]

    @staticmethod
    def of_dict(aux_dict: typing.Dict[str, typing.Any]):
        locations = aux_dict['locations']
        init = aux_dict['init']
        postcondition = aux_dict['postcondition']
        return LitmusAux(locations, init, postcondition)


@dataclass
class Aux:
    num_threads: int
    var_map: typing.Dict[str, typing.Optional[str]]
    litmus_aux: LitmusAux

    @staticmethod
    def of_dict(aux_dict: typing.Dict[str, typing.Any]):
        num_threads = aux_dict['num_threads']
        var_map = aux_dict['var_map']
        litmus_aux = LitmusAux.of_dict(aux_dict['litmus_aux'])
        return Aux(num_threads, var_map, litmus_aux)

    @staticmethod
    def load(fd: typing.TextIO) -> 'Aux':
        aux_dict: typing.Dict[str, typing.Any] = json.load(fd)
        return Aux.of_dict(aux_dict)

    @property
    def litmus_ids(self) -> typing.Iterator[litmus_id.Lid]:
        return (litmus_id.parse(k) for k in self.var_map.keys())

    def variables_of_thread(self, tid: int) -> typing.Iterator[str]:
        """Yields the C identifier of each variable in this auxiliary record that is visible from the given thread ID.

        :param tid: The ID of the thread whose local variables we want.
        :return: A generator yielding the C identifier of each global variable, or local variable of the given thread.
        """
        return (l.var for l in self.litmus_ids if l.tid is None or tid == l.tid)