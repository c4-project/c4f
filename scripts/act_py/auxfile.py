import pathlib
import typing
import json


def is_thread_var(tid: int, litmus_id: str) -> bool:
    (tid_str, sep, var) = litmus_id.partition(':')
    return sep == '' or (tid_str.strip() == str(tid))

def variables_of_thread(aux_dict: typing.Dict, tid: int) -> typing.List[str]:
    # TODO(@MattWindsor91)
    return [v for (k, v) in aux_dict["var_map"].items() if is_thread_var(tid, k)]


def get_num_threads(aux_dict: typing.Dict) -> int:
    return int(aux_dict['num_threads'])


def load(path: pathlib.Path) -> dict:
    with path.open() as f:
        return json.load(f)
