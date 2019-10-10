(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let write_aux (aux : Act_delitmus.Aux.t) (oc : Stdio.Out_channel.t) :
    unit Or_error.t =
  let aux_json = Act_delitmus.Aux.yojson_of_t aux in
  Or_error.try_with (fun () -> Yojson.Safe.pretty_to_channel oc aux_json)

let run ?(aux_output : string option)
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o _cfg
    ~(style : Act_delitmus.Runner.Style.t) =
  Common_cmd.Args.With_files.run_filter_with_aux_out
    (module Act_delitmus.Filter)
    args ~aux_in:style ~aux_out_f:write_aux ?aux_out_filename:aux_output

let command : Command.t =
  Command.basic ~summary:"convert a C litmus test to a normal C file"
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and aux_output =
        flag "aux-output"
          (optional Filename.arg_type)
          ~doc:
            "FILE if given, the filename to write auxiliary litmus \
             information to"
      and style =
        Act_delitmus.Runner.Style.(
          flag_optional_with_default_doc "style"
            (Arg_type.of_alist_exn
               [ ("vars-as-globals", Vars_as_globals)
               ; ("vars-as-parameters", Vars_as_parameters) ])
            [%sexp_of: Act_delitmus.Runner.Style.t] ~default:Vars_as_globals
            ~doc:"STYLE_NAME the style of delitmus action to run")
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~style ?aux_output))
