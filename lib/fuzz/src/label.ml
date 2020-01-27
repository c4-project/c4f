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

let labels_of_thread (thread : unit Act_c_mini.Function.t Ac.C_named.t) :
    Act_common.Litmus_id.t list =
  ignore thread ;
  (*|> C_named.value |> Act_c_mini.Function.body_stms |> List.filter *)
  []

let labels_of_test (test : Act_c_mini.Litmus.Test.t) :
    Set.M(Act_common.Litmus_id).t =
  test |> Act_c_mini.Litmus.Test.threads
  |> List.concat_map ~f:labels_of_thread
  |> Set.of_list (module Act_common.Litmus_id)
