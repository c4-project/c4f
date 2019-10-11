(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the parsing of valid config files. *)

open Core

let pp_ast : Act_config.Ast.t Fmt.t =
  Fmt.using Act_config.Ast.sexp_of_t Sexp.pp_hum

let parse_config_failures ~(file : Fpath.t) ~(path : Fpath.t) :
    unit Or_error.t =
  ignore file ;
  let r = Act_config.Frontend.load (Plumbing.Input.of_fpath path) in
  Fmt.(
    pr "@[%a@]@."
      (result ~ok:pp_ast ~error:(any "UNEXPECTED ERROR:@ " ++ Error.pp)))
    r ;
  Result.ok_unit

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "config" / "valid" / "") in
  Common.regress_on_files "Config parser (valid)" ~dir:full_dir ~ext:"conf"
    ~f:parse_config_failures

let command =
  Common.make_regress_command ~summary:"runs config parser regressions" run

let () = Command.run command
