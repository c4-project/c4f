#!/usr/bin/env python3

import argparse
import pathlib
import typing

import act_py.auxfile


def function_name_of_id(tid : int) -> str:
    return f'P{tid}'


def output_call(tid: int, variables: typing.List[str]):
    print(function_name_of_id(tid), end='(')
    print(*variables, sep=',', end=')\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('aux')

    args = parser.parse_args()

    aux_path = pathlib.Path(args.aux)
    aux = act_py.auxfile.load(aux_path)

    num_threads = act_py.auxfile.get_num_threads(aux)
    for i in range(num_threads):
        vars = act_py.auxfile.variables_of_thread(aux, i)
        output_call(i, vars)
