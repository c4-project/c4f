open Core

let iter_result f = List.fold_result ~init:() ~f:(fun _ -> f)
