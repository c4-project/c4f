(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the Herd observation parser on various Herd7-style outputs. The
    output is the corresponding ACT observation JSON, and the intent is to
    track unwanted changes in the JSON format. *)

open Core
module Ac = Act_common

let run_on_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  Or_error.Let_syntax.(
    let%bind obs =
      Act_backend_herd.Reader.load (Plumbing.Input.of_fpath path)
    in
    Act_state.Observation.store obs ~dest:Plumbing.Output.stdout)

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "observations" / "herd" / "") in
  Common.regress_on_files "Herd_observations" ~dir:full_dir ~ext:"txt"
    ~f:run_on_file

let command =
  Common.make_regress_command
    ~summary:"runs herd observation-parsing regressions" run

let () = Command.run command
