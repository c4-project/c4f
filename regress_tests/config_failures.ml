(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act c delitmus' command. *)

open Core

let parse_config_failures ~(file : Fpath.t) ~(path : Fpath.t) :
    unit Or_error.t =
  ignore file ;
  let r = Act_config.Frontend.load (Plumbing.Input.of_fpath path) in
  Fmt.(pr "@[%a@]@." (result ~ok:(always "No failures.") ~error:Error.pp)) r ;
  Ok ()

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "config" / "failures" / "") in
  Common.regress_on_files "Config parser (failures)" ~dir:full_dir
    ~ext:"conf" ~f:parse_config_failures

let command =
  Common.make_regress_command
    ~summary:"runs config parser failure regressions" run

let () = Command.run command
