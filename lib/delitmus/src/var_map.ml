(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Ac = Act_common

type t = (Ac.C_id.t option) Map.M(Ac.Litmus_id).t

let of_map : (Ac.C_id.t option) Map.M(Ac.Litmus_id).t -> t = Fn.id

let build_set (type e w)
    (module Carrier : Comparable.S with type t = e and type comparator_witness = w)
    ~(f: Ac.Litmus_id.t -> Ac.C_id.t option -> e option)
  : t -> Set.M(Carrier).t =
  Map.fold
    ~init:(Set.empty (module Carrier))
    ~f:(fun ~key ~data set ->
        Option.value_map
          (f key data)
          ~default:set
          ~f:(Set.add set))

let global_c_variables : t -> Set.M(Ac.C_id).t =
  build_set (module Ac.C_id) ~f:(fun _ data -> data)
