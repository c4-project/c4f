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
import enum
import json
import typing
from dataclasses import dataclass

from act_py import json_utils, litmus_id, litmus_header


@dataclass
class FunctionRecord:
    """Representation of a function-map record in an ACT 'aux file'."""

    is_thread_body: bool
    c_id: str

    def __str__(self) -> str:
        """Converts this aux record to a string.

        :return: A compact JSON serialisation of the aux record.
        """
        return json.dumps(dataclasses.asdict(self))


class MappingType(enum.Enum):
    """Types of mapping-to used in aux mappings."""


@dataclass
class VarRecord:
    """Representation of a var-map record in an ACT 'aux file'."""

    # Deliberately loose encoding for now, may need tightening up later.
    mapped_to: typing.List[typing.Union[str, int]]

    c_id: str
    c_type: str

    @property
    def mapped_to_global(self) -> bool:
        """Gets whether this variable has been mapped to a global variable in C."""
        return len(self.mapped_to) == 1 and self.mapped_to[0].lower() == "global"

    def __str__(self) -> str:
        """Converts this aux record to a string.

        :return: A compact JSON serialisation of the aux record.
        """
        return json.dumps(dataclasses.asdict(self))


@dataclass
class Aux:
    """Representation of an ACT 'aux file'."""

    function_map: typing.Dict[str, FunctionRecord]
    var_map: typing.Dict[str, VarRecord]
    litmus_header: litmus_header.LitmusHeader

    def __str__(self) -> str:
        """Converts this aux record to a string.

        :return: A compact JSON serialisation of the aux record.
        """
        return json.dumps(dataclasses.asdict(self))

    @property
    def num_threads(self) -> int:
        """The number of threads in the test."""
        return len([x for x in self.function_map.values() if x.is_thread_body])

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


def function_record_of_dict(fun_dict: typing.Dict[str, typing.Any]) -> FunctionRecord:
    is_thread_body = json_utils.bool_field(
        fun_dict, "is_thread_body", "function record"
    )
    c_id = json_utils.str_field(fun_dict, "c_id", "function record")
    return FunctionRecord(is_thread_body=is_thread_body, c_id=c_id)


def parse_mapped_to(input: typing.Any) -> typing.List[typing.Union[str, int]]:
    input_list = list(input)
    if not input_list:
        raise ValueError("mapped-to is an empty list")
    key = str(input_list[0]).lower()
    if key == "global":
        if len(input_list) != 1:
            raise ValueError("mapped-to is global but has arguments")
        return ["Global"]
    elif key == "param":
        if len(input_list) != 2:
            raise ValueError(
                "mapped-to is param but doesn't have strictly one argument"
            )
        return ["Param", int(input_list[1])]
    raise ValueError("mapped-to is not recognised", key)


def var_record_of_dict(vr_dict: typing.Dict[str, typing.Any]) -> VarRecord:
    mapped_to = parse_mapped_to(vr_dict["mapped_to"])
    c_id = json_utils.str_field(vr_dict, "c_id", "var record")
    c_type = json_utils.str_field(vr_dict, "c_type", "var record")
    return VarRecord(mapped_to=mapped_to, c_id=c_id, c_type=c_type)


def of_dict(aux_dict: typing.Dict[str, typing.Any]) -> Aux:
    function_map: typing.Dict[str, VarRecord] = {
        k: function_record_of_dict(v) for (k, v) in aux_dict["function_map"].items()
    }
    var_map: typing.Dict[str, VarRecord] = {
        k: var_record_of_dict(v) for (k, v) in aux_dict["var_map"].items()
    }
    header: litmus_header.LitmusHeader = litmus_header.of_dict(
        aux_dict["litmus_header"]
    )
    return Aux(function_map, var_map, header)


def load(fp: typing.TextIO) -> Aux:
    """Loads an aux file from a file pointer.

    :param fp: The (text) file pointer from which we will load an aux file.
    :return: The resulting `Aux` object.
    """
    aux_dict: typing.Dict[str, typing.Any] = json.load(fp)
    return of_dict(aux_dict)
