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
    (args : Toplevel.Args.Standard.t Toplevel.Args.With_files.t)
    (_o : Output.t) (global_cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): machine predicates? *)
    let%bind cfg = Act_config.Act.of_global global_cfg in
    let module Res = Toplevel.Sim_support.Make_resolver (struct
      let cfg = cfg
    end) in
    let%bind (module Backend) = Res.resolve_single fqid in
    Toplevel.Args.With_files.run_filter
      (module Backend.Filter)
      args ~aux_in:arch)

let command : Command.t =
  Command.basic ~summary:"runs a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args =
        ignore anon ;
        Toplevel.Args.(With_files.get Standard.get)
      and sim =
        ignore anon ;
        Toplevel.Args.simulator ()
      and arch =
        choose_one
          [ Toplevel.Args.flag_to_enum_choice (Some Act_backend.Arch.C) "-c"
              ~doc:
                "Tells the backend to test the input against the C memory \
                 model"
          ; map
              ~f:
                (Option.map ~f:(fun x -> Some (Act_backend.Arch.Assembly x)))
              (Toplevel.Args.arch
                 ~doc:
                   "ARCH tells the backend to test the input against the \
                    given architecture"
                 ()) ]
          ~if_nothing_chosen:(`Default_to None)
      in
      fun () ->
        Toplevel.Common.lift_command
          (Toplevel.Args.With_files.rest standard_args)
          ~f:(run standard_args ?arch ?fqid:sim))
