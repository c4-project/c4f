(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Common functionality for regress tests *)

open Core

val regress_on_files :
     string
  -> dir:Fpath.t
  -> ext:string
  -> f:(file:Fpath.t -> path:Fpath.t -> unit Or_error.t)
  -> unit Or_error.t

val make_regress_command :
  (Fpath.t -> unit Or_error.t) -> summary:string -> Command.t
