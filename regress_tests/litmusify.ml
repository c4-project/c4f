(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act asm litmusify' command. *)

open Core
module Ac = Act_common

let run (dir : Fpath.t) : unit Or_error.t =
  let arch = Ac.Id.of_string "x86.att" in
  Or_error.Let_syntax.(
    let%bind (module L) =
      Common_cmd.Language_support.asm_runner_of_arch arch
    in
    let module Lit = Act_asm.Litmusifier.Make (L) in
    Common.regress_run_asm_many
      (module Lit.Filter)
      "Litmusifier" Act_sanitiser.Pass_group.standard dir
      ~config_fn:(fun aux -> Act_asm.Litmusifier.Config.make ~aux ()))

let command =
  Common.make_regress_command ~summary:"runs litmusifier regressions" run

let () = Command.run command
