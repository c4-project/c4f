(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act-c dump-header' command. *)

open Core

let dump_header_of_file ~file:(_ : Fpath.t) ~(path : Fpath.t) :
    unit Or_error.t =
  Act_litmus_c.Header.Filters.run_dump
    (Plumbing.Input.of_fpath path)
    Plumbing.Output.stdout

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Dump-header" ~dir:full_dir ~ext:"litmus"
    ~f:dump_header_of_file

let command =
  Common.make_regress_command ~summary:"runs dump-header regressions" run

let () = Command.run command
