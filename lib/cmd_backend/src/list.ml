(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run
    (* ?(compiler_predicate : Act_compiler.Property.t Blang.t option) *)
      (_o : Act_common.Output.t) (_global_cfg : Act_config.Global.t) :
    unit Or_error.t =
  (* ~(with_compiler_tests : bool) *) Or_error.unimplemented "TODO"

(* let%map cfg = Common_cmd.Language_support.make_filtered_machine_config
   ?compiler_predicate ?machine_predicate ~with_compiler_tests global_cfg in
   let compilers = Act_config.Act.all_compilers cfg in let verbose =
   Common_cmd.Args.Standard.is_verbose standard_args in Fmt.(pr "@[<v>%a@]@."
   (list (pp_compiler verbose)) compilers)) *)

let command : Command.t =
  Command.(
    basic ~summary:"outputs a filtered list of backends"
      Let_syntax.(
        let%map standard_args = Common_cmd.Args.Standard.get in
        fun () -> Common_cmd.Common.lift_command standard_args ~f:run))
