(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
end

let predicate_lists : (string, (module Ac.Property_types.S)) List.Assoc.t =
  [ ("Backend predicates (-filter-backends)", (module Act_backend.Property))
  ; ("Machine predicates (-filter-machines)", (module Act_machine.Property))
  ; ("Identifier predicates", (module Ac.Id.Property)) ]

let pp_tree_module : (module Ac.Property_types.S) Fmt.t =
 fun f (module M) -> M.pp_tree f ()

(** [pp_predicate_list] is a pretty-printer for predicate lists. *)
let pp_predicate_list :
    (string, (module Ac.Property_types.S)) List.Assoc.t Fmt.t =
  Fmt.(
    list ~sep:(any "@,@,")
      (vbox ~indent:2 (pair ~sep:sp (string ++ any ":") pp_tree_module)))

let run_list_predicates (_o : Ac.Output.t) : unit Or_error.t =
  Fmt.pr "@[<v>%a@]@." pp_predicate_list predicate_lists ;
  Ok ()

let command : Command.t =
  Command.basic ~summary:"describes the filtering predicate languages"
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Args.Standard.lift_command standard_args
          ~f:run_list_predicates)
