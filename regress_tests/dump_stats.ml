(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act-c dump-stats' command. *)

open Core

let dump_stats_of_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t
    =
  ignore file ;
  Act_litmus_c.Dump_stats.Filter.run ()
    (Plumbing.Input.of_fpath path)
    Plumbing.Output.stdout

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Dump-stats" ~dir:full_dir ~ext:"litmus"
    ~f:dump_stats_of_file

let command =
  Common.make_regress_command ~summary:"runs dump-stats regressions" run

let () = Command.run command
