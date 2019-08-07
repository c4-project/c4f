(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

type t = Ac.Litmus_id.t option Map.M(Ac.Litmus_id).t

module Json =
  Plumbing.Jsonable.Make_map
    (Ac.Litmus_id)
    (Plumbing.Jsonable.Option (Ac.Litmus_id))

include (Json : module type of Json with type t := t)

module Load = Plumbing.Loadable.Of_jsonable (Json)

include (Load : module type of Load with type t := t)

let reflexive (vars : Set.M(Act_common.Litmus_id).t) : t =
  vars
  |> Set.to_sequence ~order:`Increasing
  |> Sequence.map ~f:(fun x -> (x, Some x))
  |> Map.of_increasing_sequence (module Act_common.Litmus_id)
  |> Or_error.ok_exn

let output (map : t) ~(onto : Plumbing.Output.t) : unit Or_error.t =
  Plumbing.Output.with_output onto ~f:(fun oc ->
      map |> to_yojson |> Yojson.Safe.pretty_to_channel oc ;
      Stdio.Out_channel.newline oc ;
      Result.ok_unit)
