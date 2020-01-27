(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

module Stm = Act_c_mini.Statement.With_meta (Unit)

let labels_of_thread (tid : int)
    (thread : unit Act_c_mini.Function.t Ac.C_named.t) :
    Act_common.Litmus_id.t list =
  thread |> Ac.C_named.value |> Act_c_mini.Function.body_stms
  |> List.concat_map ~f:Stm.On_primitives.to_list
  |> List.filter_map ~f:Act_c_mini.Prim_statement.as_label
  |> List.map ~f:(Act_common.Litmus_id.local tid)

let labels_of_test (test : Act_c_mini.Litmus.Test.t) :
    Set.M(Act_common.Litmus_id).t =
  test |> Act_c_mini.Litmus.Test.threads
  |> List.concat_mapi ~f:labels_of_thread
  |> Set.of_list (module Act_common.Litmus_id)

let gen_fresh (set : Set.M(Ac.Litmus_id).t) :
    Ac.C_id.t Base_quickcheck.Generator.t =
  let flat_set =
    Set.map (module Ac.C_id) ~f:Ac.Litmus_id.variable_name set
  in
  Base_quickcheck.Generator.(
    Ac.C_id.Human.quickcheck_generator
    |> filter ~f:(Fn.non (Set.mem flat_set)))
