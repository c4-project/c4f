(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common
module C_spec = Act_compiler.Spec
module Cq_spec = Act_machine.Qualified.Compiler

let predicate_lists : (string, (module Property_types.S)) List.Assoc.t =
  [ ("Backend predicates (-filter-backends)", (module Act_compiler.Property))
  ; ( "Compiler predicates (-filter-compilers)"
    , (module Act_compiler.Property) )
  ; ("Machine predicates (-filter-machines)", (module Act_machine.Property))
  ; ("Identifier predicates", (module Id.Property)) ]

let pp_tree_module : (module Property_types.S) Fmt.t =
 fun f (module M) -> M.pp_tree f ()

(** [pp_predicate_list] is a pretty-printer for predicate lists. *)
let pp_predicate_list :
    (string, (module Property_types.S)) List.Assoc.t Fmt.t =
  Fmt.(
    list ~sep:(any "@,@,")
      (vbox ~indent:2 (pair ~sep:sp (string ++ any ":") pp_tree_module)))

let run_list_predicates (_o : Output.t) (_cfg : Act_config.Global.t) :
    unit Or_error.t =
  Fmt.pr "@[<v>%a@]@." pp_predicate_list predicate_lists ;
  Ok ()

let list_predicates_command : Command.t =
  Command.basic ~summary:"describes the filtering predicate languages"
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Common.lift_command standard_args ~f:run_list_predicates)

let command : Command.t =
  Command.group ~summary:"commands for dealing with ACT configuration"
    [ ("dump", Dump.command)
    ; ("license", Common_cmd.License.command)
    ; ("list-predicates", list_predicates_command) ]
