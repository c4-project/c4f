(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the fuzz trace replayer on various pairs of litmus test and trace.
    The test tracks every individual trace in the directory, and derives the
    corresponding test by stripping all extensions and adding '.litmus'. The
    output is the fuzzed output, and the intent is to track unwanted changes
    in action replays. *)

open Core

let litmus_path_of (trace_path : Fpath.t) : Fpath.t =
  trace_path |> Fpath.rem_ext ~multi:true |> Fpath.add_ext "litmus"

let run_on_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  let litmus_path = litmus_path_of path in
  Or_error.Let_syntax.(
    let%bind trace = Act_fuzz.Trace.load (Plumbing.Input.of_fpath path) in
    let aux = Act_fuzz_run.Filter.Aux.Replay.make trace in
    Act_fuzz_run.Filter.Replay.run aux
      (Plumbing.Input.of_fpath litmus_path)
      Plumbing.Output.stdout)

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "fuzz" / "replays" / "") in
  Common.regress_on_files "Fuzz replay" ~dir:full_dir ~ext:"trace"
    ~f:run_on_file

let command =
  Common.make_regress_command
    ~summary:"runs litmus observation-parsing regressions" run

let () = Command.run command
