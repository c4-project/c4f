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
    (args : Args.Standard.t Args.With_files.t) (_o : Output.t)
    (cfg : Act_config.Act.t) : unit Or_error.t =
  let module Res = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind input = Args.With_files.infile_source args in
    let%bind output = Args.With_files.outfile_sink args in
    let%bind (module Sim) = Res.resolve_single fqid in
    let%bind input_path = Plumbing.Input.to_file_err input in
    let%bind output_dir = Plumbing.Output.to_file_err output in
    let%map cmds = Sim.make_harness arch ~input_path ~output_dir in
    List.iter ~f:Stdio.print_endline cmds)

let command : Command.t =
  Command.basic
    ~summary:"runs a configured test backend in harness-making mode"
    Command.Let_syntax.(
      let%map_open standard_args =
        ignore anon ;
        Args.(With_files.get Standard.get)
      and sim = ignore anon ; Args.simulator ()
      and arch =
        choose_one
          [ Args.flag_to_enum_choice (Some Act_backend.Arch.C) "-c"
              ~doc:
                "Tells the backend to test the input against the C memory \
                 model"
          ; map
              ~f:(Option.map ~f:(fun x -> Some (Act_backend.Arch.Assembly x)))
              (Args.arch
                 ~doc:
                   "ARCH tells the backend to test the input against the \
                    given architecture"
                 ()) ]
          ~if_nothing_chosen:(`Default_to None)
      in
      fun () ->
        Common.lift_command
          (Args.With_files.rest standard_args)
          ~with_compiler_tests:false
          ~f:(run standard_args ?arch ?fqid:sim))
