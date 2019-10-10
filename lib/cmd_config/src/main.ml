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

let pp_compiler_verbose (f : Formatter.t) (spec : Cq_spec.t) : unit =
  let compiler = Cq_spec.c_spec spec in
  Fmt.pf f "@[<v 2>@[<h>%a/%a@]@ %a@]" Id.pp
    (Act_machine.Spec.With_id.id (Cq_spec.m_spec spec))
    Id.pp
    (C_spec.With_id.id compiler)
    C_spec.pp
    (C_spec.With_id.spec compiler)

let pp_compiler_terse (f : Formatter.t) (spec : Cq_spec.t) : unit =
  let compiler = Cq_spec.c_spec spec in
  Fmt.pf f "@[<h>%a@ %a@ %a@]" Id.pp
    (Act_machine.Spec.With_id.id (Cq_spec.m_spec spec))
    Id.pp
    (C_spec.With_id.id compiler)
    C_spec.pp_summary
    (C_spec.With_id.spec compiler)

let pp_compiler (verbose : bool) : Cq_spec.t Fmt.t =
  if verbose then pp_compiler_verbose else pp_compiler_terse

let run_list_compilers
    ?(compiler_predicate : Act_compiler.Property.t Blang.t option)
    ?(machine_predicate : Act_machine.Property.t Blang.t option)
    (standard_args : Common_cmd.Args.Standard.t) (_o : Output.t)
    (global_cfg : Act_config.Global.t) ~(with_compiler_tests : bool) :
    unit Or_error.t =
  Or_error.Let_syntax.(
    let%map cfg =
      Common_cmd.Language_support.make_filtered_machine_config
        ?compiler_predicate ?machine_predicate ~with_compiler_tests
        global_cfg
    in
    let compilers = Act_config.Act.all_compilers cfg in
    let verbose = Common_cmd.Args.Standard.is_verbose standard_args in
    Fmt.(pr "@[<v>%a@]@." (list (pp_compiler verbose)) compilers))

let list_compilers_command : Command.t =
  Command.basic
    ~summary:"outputs information about the current compiler specs"
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and compiler_predicate = Common_cmd.Args.compiler_predicate
      and machine_predicate = Common_cmd.Args.machine_predicate
      and with_compiler_tests =
        flag "test-compilers" no_arg
          ~doc:
            "If true, test each compiler's presence and only print the \
             compilers that pass"
      in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:
            (run_list_compilers standard_args ?compiler_predicate
               ?machine_predicate ~with_compiler_tests))

let predicate_lists : (string, (module Property_types.S)) List.Assoc.t =
  [ ( "Compiler predicates (-filter-compilers)"
    , (module Act_compiler.Property) )
  ; ("Machine predicates (-filter-machines)", (module Act_machine.Property))
  ; ("Identifier predicates", (module Id.Property))
  ; ( "Sanitiser passes (-sanitiser-passes)"
    , (module Act_sanitiser.Pass_group.Selector) ) ]

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
  Result.ok_unit

let list_predicates_command : Command.t =
  Command.basic ~summary:"describes the filtering predicate languages"
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Common.lift_command standard_args ~f:run_list_predicates)

let command : Command.t =
  Command.group ~summary:"commands for dealing with ACT configuration"
    [ ("license", Common_cmd.License.command)
    ; ("list-compilers", list_compilers_command)
    ; ("list-predicates", list_predicates_command) ]
