(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests the output of the 'act-c gen-locmap' command for generating
    reflexive diff location maps. *)

open Core
module Ac = Act_common

let gen_locmap_for_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  Or_error.(
    (* TODO(@MattWindsor91): this duplicates a lot of the command it's
       supposed to be testing. *)
    path
    |> Plumbing.Input.of_fpath
    |> Act_c_mini.Frontend.load_from_isrc
    >>| Act_c_mini.Litmus.vars
    >>| Act_backend.Diff.Location_map.reflexive
    >>= Act_backend.Diff.Location_map.output ~onto:Plumbing.Output.stdout
  )

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Reflexive-locmap" ~dir:full_dir ~ext:"litmus"
    ~f:gen_locmap_for_file

let command =
  Common.make_regress_command ~summary:"runs delitmusifier regressions" run

let () = Command.run command
