(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let write_aux (output : C4f_delitmus.Output.t) (oc : Stdio.Out_channel.t) :
    unit Or_error.t =
  let aux = C4f_delitmus.Output.aux output in
  let aux_json = C4f_delitmus.Aux.yojson_of_t aux in
  Or_error.try_with (fun () -> Yojson.Safe.pretty_to_channel oc aux_json)

let run ?(aux_output : Plumbing.Output.t option)
    ?(impl_suffix : string option)
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t) _o
    ~(style : C4f_delitmus.Config.Style.t) ~(no_qualify_locals : bool) =
  let qualify_locals = not no_qualify_locals in
  let config =
    C4f_delitmus.Config.make ?impl_suffix ~qualify_locals ~style ()
  in
  Or_error.Let_syntax.(
    let%bind aux_out =
      Common_cmd.Args.With_files.run_filter
        (C4f_delitmus.Filter.run ~config)
        args
    in
    Plumbing.Output.with_output_opt aux_output ~f:(write_aux aux_out))

let command : Command.t =
  Command.basic ~summary:"convert a C litmus test to a normal C file"
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and aux_output =
        flag "aux-output"
          (optional Common_cmd.Args.output_type)
          ~doc:
            "FILE if given, the filename to write auxiliary litmus \
             information to"
      and style =
        C4f_delitmus.Config.Style.(
          flag_optional_with_default_doc "style"
            (Arg_type.of_alist_exn
               [ ("vars-as-globals", Vars_as_globals)
               ; ("vars-as-parameters", Vars_as_parameters) ] )
            [%sexp_of: C4f_delitmus.Config.Style.t] ~default:Vars_as_globals
            ~doc:"STYLE_NAME the style of delitmus action to run")
      and impl_suffix =
        flag "impl-suffix" (optional string)
          ~doc:"STRING append this to names of thread functions"
      and no_qualify_locals =
        flag "no-qualify-locals" no_arg
          ~doc:"if given, don't thread-ID-qualify local variable names"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:
            (run standard_args ~style ~no_qualify_locals ?impl_suffix
               ?aux_output ))
