(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Lists the available fuzz actions, parameters and flags, and their default
    values; intended to track unwanted changes to these tables. *)

open Core

(* TODO(@MattWindsor91): merge with `act-fuzz list_actions`? *)

let weight_summary (c : C4f_fuzz_run.Config.t) : unit Or_error.t =
  Stdio.print_endline "# Weights" ;
  let ws = C4f_fuzz_run.Config.Weight_summary.summarise c in
  C4f_utils.My_format.fdump Stdio.stdout
    C4f_fuzz_run.Config.Weight_summary.pp ws ;
  Ok ()

let param_summary (c : C4f_fuzz_run.Config.t) : unit Or_error.t =
  Stdio.print_endline "# Params" ;
  Or_error.Let_syntax.(
    let%map ps = C4f_fuzz_run.Config.Param_summary.summarise c in
    C4f_utils.My_format.fdump Stdio.stdout
      C4f_fuzz_run.Config.Param_summary.pp ps)

let run (_test_dir : Fpath.t) : unit Or_error.t =
  let c = C4f_fuzz_run.Config.make () in
  Or_error.all_unit [weight_summary c; param_summary c]

let command : Command.t =
  Common.make_regress_command ~summary:"runs fuzz list regressions" run

let () = Command.run command
