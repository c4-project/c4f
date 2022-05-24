(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act c delitmus' command. *)

open Core

let parse_config_failures ~(file : Fpath.t) ~(path : Fpath.t) :
    unit Or_error.t =
  ignore file ;
  let r = C4f_config.Frontend.load (Plumbing.Input.of_fpath path) in
  Fmt.(pr "@[%a@]@." (result ~ok:(any "No failures.") ~error:Error.pp)) r ;
  Ok ()

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "config" / "failures" / "") in
  Common.regress_on_files "Config parser (failures)" ~dir:full_dir
    ~ext:"conf" ~f:parse_config_failures

let command =
  Common.make_regress_command
    ~summary:"runs config parser failure regressions" run

let () = Command_unix.run command
