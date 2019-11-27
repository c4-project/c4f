(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common

let run ?(arch = Act_backend.Arch.C) ?(fqid : Id.t = Id.of_string "herd")
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t)
    (_o : Output.t) (cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind input = Common_cmd.Args.With_files.infile_source args in
    let%bind output = Common_cmd.Args.With_files.outfile_sink args in
    let%bind input_path = Plumbing.Input.to_fpath_err input in
    let%bind output_dir = Plumbing.Output.to_fpath_err output in
    let%bind (module Backend : Act_backend.Runner_types.S) =
      Common_cmd.Backend_support.lookup_and_resolve_in_cfg fqid ~cfg
    in
    let%map cmds = Backend.make_harness arch ~input_path ~output_dir in
    List.iter ~f:Stdio.print_endline cmds)

let command : Command.t =
  Command.basic ~summary:"ask a configured test backend to make a harness"
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and sim = Common_cmd.Args.simulator ()
      and arch =
        choose_one
          [ Common_cmd.Args.flag_to_enum_choice Act_backend.Arch.C "-c"
              ~doc:
                "Tells the backend to test the input against the C memory \
                 model"
          ; map
              ~f:(Option.map ~f:(fun x -> Act_backend.Arch.Assembly x))
              (Common_cmd.Args.arch
                 ~doc:
                   "ARCH tells the backend to test the input against the \
                    given architecture"
                 ()) ]
          ~if_nothing_chosen:Return_none
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?arch ?fqid:sim))
