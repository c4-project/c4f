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

let run (standard_args : Common_cmd.Args.Standard.t) (_o : Ac.Output.t)
    (global_cfg : Act_config.Global.t) ~(machines : Act_common.Id.t list)
    ~(style : Act_common.Id.t) ~(with_backend_tests : bool) : unit Or_error.t
    =
  ignore machines ;
  ignore standard_args ;
  ignore global_cfg ;
  ignore style ;
  ignore with_backend_tests ;
  Or_error.unimplemented "TODO(@MattWindsor91)"

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Given a style identifier (for example, 'herd' or 'litmus') and zero
      or more target machines, this command tries to find the ID of a backend
      that implements that style.
    |}

let command : Command.t =
  Command.basic
    ~summary:"finds the most appropriate backend for a particular style"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and style = anon ("STYLE_ID" %: Common_cmd.Args.id_type)
      and machines =
        anon
          (non_empty_sequence_as_list
             ("MACHINE_ID" %: Common_cmd.Args.id_type))
      and with_backend_tests =
        flag "test-backends" no_arg
          ~doc:
            "If true, test each backend's presence and only print the \
             backends that pass"
      in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:(run standard_args ~style ~machines ~with_backend_tests))
