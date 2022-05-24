(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests that the Litmus/C parser gives the appropriate error. *)

open Core

let fail_to_parse_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t
    =
  ignore file ;
  let result =
    C4f_litmus_c.Frontend.Litmus.load (Plumbing.Input.of_fpath path)
  in
  Fmt.(pr "@[%a@]@." (result ~ok:(any "(success)") ~error:Error.pp)) result ;
  Ok ()

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "failures" / "") in
  Common.regress_on_files "Litmus-failures" ~dir:full_dir ~ext:"litmus"
    ~f:fail_to_parse_file

let command =
  Common.make_regress_command ~summary:"checks parser failure messages" run

let () = Command_unix.run command
