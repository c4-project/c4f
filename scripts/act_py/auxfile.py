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
import dataclasses
import pathlib
import typing
import json
from dataclasses import dataclass

from . import litmus_id


@dataclass
class LitmusAux:
    """Representation of the part of an ACT 'aux file' that contains Litmus-specific information."""

    locations: typing.Optional[typing.List[str]]
    init: typing.Optional[typing.Dict[str, int]]
    postcondition: typing.Optional[str]

    def rewrite_locals(self, rewriter: typing.Callable[[litmus_id.Lid], str]):
        if self.postcondition is not None:
            self.postcondition = litmus_id.rewrite_post_locals(
                self.postcondition, rewriter
            )


@dataclass
class VarRecord:
    """Representation of a var-map record in an ACT 'aux file'."""

    is_global: bool
    c_id: str
    c_type: str

    def __str__(self) -> str:
        """Converts this aux record to a string.

        :return: A compact JSON serialisation of the aux record.
        """
        return json.dumps(dataclasses.asdict(self))


@dataclass
class Aux:
    """Representation of an ACT 'aux file'."""

    num_threads: int
    var_map: typing.Dict[str, VarRecord]
    litmus_aux: LitmusAux

    def __str__(self) -> str:
        """Converts this aux record to a string.

        :return: A compact JSON serialisation of the aux record.
        """
        return json.dumps(dataclasses.asdict(self))

    @property
    def litmus_ids(self) -> typing.Iterator[litmus_id.Lid]:
        return (litmus_id.parse(k) for k in self.var_map.keys())

    def dump(self, fp: typing.TextIO) -> None:
        """Dumps this aux record to a file.

        :param fp: A file-like object to use as the target of the write.
        :return: Nothing.
        """
        json.dump(dataclasses.asdict(self), fp, indent="\t")

    def variables_of_thread(self, tid: int) -> typing.Iterator[litmus_id.Lid]:
        """Yields the C identifier of each variable in this auxiliary record that is visible from the given thread ID.

        :param tid: The ID of the thread whose local variables we want.
        :return: A generator yielding the Litmus identifier of each global variable, or local variable of the given
                 thread.
        """
        return (l for l in self.litmus_ids if l.tid is None or tid == l.tid)

    def rewrite_locals(self, rewriter: typing.Callable[[litmus_id.Lid], str]):
        self.litmus_aux.rewrite_locals(rewriter)


def litmus_of_dict(aux_dict: typing.Dict[str, typing.Any]) -> LitmusAux:
    locations = aux_dict["locations"]
    init = aux_dict["init"]
    postcondition = aux_dict["postcondition"]
    return LitmusAux(locations, init, postcondition)


def var_record_of_dict(vr_dict: typing.Dict[str, typing.Any]) -> VarRecord:
    is_global = bool(vr_dict["is_global"])
    c_id = str(vr_dict["c_id"])
    c_type = str(vr_dict["c_type"])
    return VarRecord(is_global=is_global, c_id=c_id, c_type=c_type)


def of_dict(aux_dict: typing.Dict[str, typing.Any]) -> Aux:
    num_threads: int = aux_dict["num_threads"]
    var_map: typing.Dict[str, VarRecord] = {
        k: var_record_of_dict(v) for (k, v) in aux_dict["var_map"].items()
    }
    litmus_aux: LitmusAux = litmus_of_dict(aux_dict["litmus_aux"])
    return Aux(num_threads, var_map, litmus_aux)


def load(fp: typing.TextIO) -> Aux:
    """Loads an aux file from a file pointer.

    :param fp: The (text) file pointer from which we will load an aux file.
    :return: The resulting `Aux` object.
    """
    aux_dict: typing.Dict[str, typing.Any] = json.load(fp)
    return of_dict(aux_dict)
