#!/usr/bin/env python3

import argparse
import pathlib

import act_py.auxfile


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
    aux = act_py.auxfile.load(aux_path)
    num_threads = act_py.auxfile.get_num_threads(aux)

    for i in range(num_threads):
        output_thread(i)
