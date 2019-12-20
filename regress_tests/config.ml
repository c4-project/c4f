(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the parsing and interpretation of valid config files. *)

open Core

let parse_config ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  let r = path |> Plumbing.Input.of_fpath |> Act_config.Global.Load.load in
  Fmt.(
    pr "@[%a@]@."
      (result
         ~ok:(vbox Act_config.Reify.pp)
         ~error:(any "UNEXPECTED ERROR:@ " ++ Error.pp)))
    r ;
  Ok ()

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "config" / "valid" / "") in
  Common.regress_on_files "Config parser (valid)" ~dir:full_dir ~ext:"conf"
    ~f:parse_config

let command =
  Common.make_regress_command ~summary:"runs config parser regressions" run

let () = Command.run command
