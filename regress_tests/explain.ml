(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act asm explain' command. *)

open Core
module Ac = Act_common

let run (dir : Fpath.t) : unit Or_error.t =
  let arch = Ac.Id.of_string "x86.att" in
  Or_error.Let_syntax.(
    let%bind (module L) =
      Common_cmd.Language_support.asm_runner_of_arch arch
    in
    let module Exp = Act_asm.Explainer.Make (L) in
    Common.regress_run_asm_many
      (module Exp.Filter)
      "Explainer" Act_sanitiser.Pass_group.explain dir
      ~config_fn:(fun aux -> Act_asm.Explainer.Config.make ~aux ()))

let command =
  Common.make_regress_command ~summary:"runs explainer regressions" run

let () = Command.run command
