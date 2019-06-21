#!/usr/bin/env python3

import argparse
import typing

import act_py.auxfile
import act_py.litmus_id


def function_name_of_id(tid : int) -> str:
    return f'P{tid}'


def call_string(tid: int, variables: typing.Iterable[str]) -> str:
    """
    Generates a string corresponding to a C function call.

    >>> call_string(5, [])
    'P5();'
    >>> call_string(10, ['ham', 'eggs', 'spam'])
    'P5(ham, eggs, spam);'

    :param tid: the ID of the thread to call
    :param variables: iterable sequence of variables to place into the call arguments verbatim
    :return: a string of C syntax representing the call
    """
    name: str = function_name_of_id(tid)
    body: str = ', '.join(variables)
    return f'{name}({body});'

def output_calls(aux: act_py.auxfile.Aux):
    # TODO(@MattWindsor91): change the variables to the correct litmus-fu.
    call_strs = [ call_string(i, aux.variables_of_thread(i)) for i in range(aux.num_threads)]
    print(*call_strs, sep='\n// NEXT\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('aux')

    args = parser.parse_args()

    aux: act_py.auxfile.Aux = act_py.auxfile.load_path(args.aux)
    output_calls(aux)
