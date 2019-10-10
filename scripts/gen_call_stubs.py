#!/usr/bin/env python3


import argparse
import typing

import act_py.args
import act_py.auxfile
import act_py.litmus_id


def function_name_of_id(tid: int) -> str:
    return f"P{tid}_body"


def call_string(tid: int, variables: typing.Iterable[str]) -> str:
    """Generates a string corresponding to a C function call.

    >>> call_string(5, [])
    'P5();'
    >>> call_string(10, ['ham', 'eggs', 'spam'])
    'P10(ham, eggs, spam);'

    :param tid: the ID of the thread to call
    :param variables: iterable sequence of variables to place into the call arguments verbatim
    :return: a string of C syntax representing the call
    """
    name: str = function_name_of_id(tid)
    body: str = ", ".join(variables)
    return f"{name}({body});"


def argument_of_var(var: act_py.litmus_id.Lid) -> str:
    """Converts a variable name into a string of C syntax accessing corresponding Litmus7 test harness location.

    >>> argument_of_var(act_py.litmus_id.Lid(None, 'spam'))
    '&(_a->spam[_i])'
    >>> argument_of_var(act_py.litmus_id.Lid(0, 'eggs'))
    '&(_a->t0eggs[_i])'

    :param var: the name of the variable to transform
    :return: a string of C syntax representing the corresponding call argument
    """
    return f"&(_a->{var.qualified}[_i])"


def arguments_of_thread(tid: int, aux: act_py.auxfile.Aux) -> typing.Iterator[str]:
    """Generates a litmus7-compatible argument list for the given thread and aux record

    :param tid: The thread whose arguments are to be iterated.
    :param aux: The aux record supplying the argument information.
    :return: An iterable of litmus7-compatible arguments.
    """
    return (argument_of_var(v) for v in aux.variables_of_thread(tid))


def output_calls(aux: act_py.auxfile.Aux):
    call_strings = (
        call_string(i, arguments_of_thread(i, aux)) for i in range(aux.num_threads)
    )
    print(*call_strings, sep="\n// NEXT\n", end="\n// END\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(parents=[act_py.args.aux_in_parser])
    args = parser.parse_args()

    aux: act_py.auxfile.Aux = act_py.auxfile.load(args.aux)
    output_calls(aux)
