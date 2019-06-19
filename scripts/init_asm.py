#!/usr/bin/env python3

import argparse
import json
import pathlib
import typing


def get_num_threads(aux_dict: typing.Dict) -> int:
    return int(aux_dict['num_threads'])


def load_aux(path: pathlib.Path) -> dict:
    with path.open() as f:
        return json.load(f)


def label_of_id(id : int) -> str:
    return f'P{id}'


def output_thread(id : int):
    print(label_of_id(id), ':', sep='')
    print("\tmfence")


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('aux')

    args = parser.parse_args()

    aux_path = pathlib.Path(args.aux)
    aux = load_aux(aux_path)
    num_threads = get_num_threads(aux)

    for i in range(num_threads):
        output_thread(i)
