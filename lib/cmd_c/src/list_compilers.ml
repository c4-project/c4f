(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common

let pp_compiler_verbose (f : Formatter.t) (spec : Act_machine.Qualified.Compiler.t) : unit =
  let compiler = Act_machine.Qualified.spec spec in
  Fmt.pf f "@[<v 2>@[<h>%a/%a@]@ %a@]" Ac.Id.pp
    (Act_machine.Spec.With_id.id (Act_machine.Qualified.m_spec spec))
    Ac.Id.pp
    (Ac.Spec.With_id.id compiler)
    Act_compiler.Spec.pp
    (Ac.Spec.With_id.spec compiler)

let run
    ?(compiler_predicate : Act_compiler.Property.t Blang.t option)
    ?(machine_predicate : Act_machine.Property.t Blang.t option)
    (standard_args : Common_cmd.Args.Standard.t) (_o : Ac.Output.t)
    (global_cfg : Act_config.Global.t) ~(with_compiler_tests : bool) :
    unit Or_error.t =
  (* TODO(@MattWindsor91): print out information about failed compilers too. *)
  Or_error.Let_syntax.(
    let%map compiler_listing =
      Common_cmd.Language_support.Lookup.filtered_list
        ?predicate:compiler_predicate ?machine_predicate
        ~test_specs:with_compiler_tests
        (Act_config.Global.machines global_cfg)
    in
    let to_enabled_list = Fn.compose
      Act_common.Spec.Set.On_specs.to_list
      Act_machine.Lookup_listing.enabled in
    let verbose = Common_cmd.Args.Standard.is_verbose standard_args in
    let pp =
      if verbose then Fmt.(using to_enabled_list (list pp_compiler_verbose))
      else Act_machine.Lookup_listing.pp_qualified_summary Act_compiler.Spec.pp_summary
    in
    Fmt.(pr "@[<v>%a@]@." pp compiler_listing))

let command : Command.t =
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
            (run standard_args ?compiler_predicate
               ?machine_predicate ~with_compiler_tests))
