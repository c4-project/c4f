import pathlib
import typing
import json


def get_num_threads(aux_dict: typing.Dict) -> int:
    return int(aux_dict['num_threads'])


def load(path: pathlib.Path) -> dict:
    with path.open() as f:
        return json.load(f)
