import pathlib
import typing
import json
from dataclasses import dataclass


def is_thread_var(tid: int, litmus_id: str) -> bool:
    (tid_str, sep, var) = litmus_id.partition(':')
    return sep == '' or (tid_str.strip() == str(tid))


@dataclass
class LitmusAux:
    locations: typing.Optional[typing.List[str]]
    init: typing.Optional[typing.Dict[str,int]]
    postcondition: typing.Optional[str]

    @staticmethod
    def of_dict(aux_dict: typing.Dict[str, typing.Any]):
        locations = aux_dict['locations']
        init = aux_dict['init']
        postcondition = aux_dict['postcondition']
        return LitmusAux(locations, init, postcondition)


@dataclass
class Aux:
    num_threads: int
    var_map: typing.Dict[str, typing.Optional[str]]
    litmus_aux: LitmusAux

    @staticmethod
    def of_dict(aux_dict : typing.Dict[str, typing.Any]):
        num_threads = aux_dict['num_threads']
        var_map = aux_dict['var_map']
        litmus_aux = LitmusAux.of_dict(aux_dict['litmus_aux'])
        return Aux(num_threads, var_map, litmus_aux)

    @staticmethod
    def load(path: pathlib.Path) -> 'Aux':
        with path.open() as f:
            aux_dict: typing.Dict = json.load(f)
        return Aux.of_dict(aux_dict)

    def variables_of_thread(self, tid: int) -> typing.List[str]:
        # TODO(@MattWindsor91)
        return [k for (k, _) in self.var_map.items() if is_thread_var(tid, k)]
