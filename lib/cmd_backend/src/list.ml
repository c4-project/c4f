(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
end

let pp_listing (verbose : bool) :
    Act_backend.Spec.t Act_machine.Qualified.t Act_machine.Lookup_listing.t
    Fmt.t =
  if verbose then
    Act_machine.Lookup_listing.pp_qualified_verbose ~type_str:"backends"
      Act_backend.Spec.pp
  else
    Act_machine.Lookup_listing.pp_qualified_summary
      Act_backend.Spec.pp_summary

let run ?(backend_predicate : Act_backend.Property.t Blang.t option)
    ?(machine_predicate : Act_machine.Property.t Blang.t option)
    (standard_args : Common_cmd.Args.Standard.t) (_o : Ac.Output.t)
    (global_cfg : Act_config.Global.t) ~(with_backend_tests : bool) :
    unit Or_error.t =
  (* TODO(@MattWindsor91): print out information about failed backends too. *)
  Or_error.Let_syntax.(
    let%map backend_listing =
      Common_cmd.Backend_support.Lookup.filtered_list
        ?predicate:backend_predicate ?machine_predicate
        ~test_specs:with_backend_tests
        (Act_config.Global.machines global_cfg)
    in
    let verbose = Common_cmd.Args.Standard.is_verbose standard_args in
    Fmt.(pr "@[<v>%a@]@." (pp_listing verbose) backend_listing))

let command : Command.t =
  Command.basic
    ~summary:"outputs information about the current backend specs"
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and backend_predicate = Common_cmd.Args.backend_predicate
      and machine_predicate = Common_cmd.Args.machine_predicate
      and with_backend_tests =
        flag "test-backends" no_arg
          ~doc:
            "If true, test each backend's presence and only print the \
             backends that pass"
      in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:
            (run standard_args ?backend_predicate ?machine_predicate
               ~with_backend_tests))
