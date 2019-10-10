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

import dataclasses
import json
import typing
from dataclasses import dataclass

from act_py import json_utils, litmus_id, litmus_header


@dataclass
class VarRecord:
    """Representation of a var-map record in an ACT 'aux file'."""

    mapped_to_global: bool
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
    litmus_header: litmus_header.LitmusHeader

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
        json_utils.dump_dataclass(self, fp)

    def variables_of_thread(self, tid: int) -> typing.Iterator[litmus_id.Lid]:
        """Yields the C identifier of each variable in this auxiliary record that is visible from the given thread ID.

        :param tid: The ID of the thread whose local variables we want.
        :return: A generator yielding the Litmus identifier of each global variable, or local variable of the given
                 thread.
        """
        return (l for l in self.litmus_ids if l.tid is None or tid == l.tid)

    def rewrite_locals(self, rewriter: typing.Callable[[litmus_id.Lid], str]):
        self.litmus_header.rewrite_locals(rewriter)


def var_record_of_dict(vr_dict: typing.Dict[str, typing.Any]) -> VarRecord:
    mapped_to_global = json_utils.bool_field(vr_dict, "mapped_to_global", "var record")
    c_id = json_utils.str_field(vr_dict, "c_id", "var record")
    c_type = json_utils.str_field(vr_dict, "c_type", "var record")
    return VarRecord(mapped_to_global=mapped_to_global, c_id=c_id, c_type=c_type)


def of_dict(aux_dict: typing.Dict[str, typing.Any]) -> Aux:
    num_threads: int = aux_dict["num_threads"]
    var_map: typing.Dict[str, VarRecord] = {
        k: var_record_of_dict(v) for (k, v) in aux_dict["var_map"].items()
    }
    header: litmus_header.LitmusHeader = litmus_header.of_dict(
        aux_dict["litmus_header"]
    )
    return Aux(num_threads, var_map, header)


def load(fp: typing.TextIO) -> Aux:
    """Loads an aux file from a file pointer.

    :param fp: The (text) file pointer from which we will load an aux file.
    :return: The resulting `Aux` object.
    """
    aux_dict: typing.Dict[str, typing.Any] = json.load(fp)
    return of_dict(aux_dict)
