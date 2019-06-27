(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common

let run ?(arch = Act_sim.Arch.C) ?(fqid : Id.t = Id.of_string "herd")
    (args : Args.Standard_with_files.t) (_o : Output.t)
    (cfg : Act_config.Act.t) : unit Or_error.t =
  let module Res = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind input = Args.Standard_with_files.infile_source args in
    let%bind output = Args.Standard_with_files.outfile_sink args in
    let%bind (module Sim) = Res.resolve_single fqid in
    Sim.Filter.run arch input output)

let command : Command.t =
  Command.basic ~summary:"runs a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      and sim = Args.simulator ()
      and arch =
        choose_one
          [ Args.flag_to_enum_choice (Some Act_sim.Arch.C) "-c"
              ~doc:
                "Tells the backend to test the input against the C memory \
                 model"
          ; map
              ~f:(Option.map ~f:(fun x -> Some (Act_sim.Arch.Assembly x)))
              (Args.arch
                 ~doc:
                   "ARCH tells the backend to test the input against the \
                    given architecture"
                 ()) ]
          ~if_nothing_chosen:(`Default_to None)
      in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false ~f:(run ?arch ?fqid:sim))
