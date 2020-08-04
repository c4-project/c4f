(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Lists the available fuzz actions, their default weights, and their names
    and READMEs. Intended to track unwanted changes to the action pool. *)

open Core

(* TODO(@MattWindsor91): merge with `act-fuzz list_actions`? *)

let run (_test_dir : Fpath.t) : unit Or_error.t =
  Or_error.(
    Act_fuzz_run.Config.(make () |> summarise)
    >>| Fmt.pr "@[<v>%a@]@." Act_fuzz.Action.Summary.pp_map)

let command : Command.t =
  Common.make_regress_command ~summary:"runs fuzz action list regressions"
    run

let () = Command.run command
