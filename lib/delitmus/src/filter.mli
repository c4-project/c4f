(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** C delitmusifying as a filter over Litmus files. *)

val run :
     Plumbing.Input.t
  -> Plumbing.Output.t
  -> config:Config.t
  -> Output.t Or_error.t
(** [run i o ~config] runs a delitmus pass over the Litmus arriving through
    [i], behaving according to [config], outputting to [o], and returning
    auxiliary output. *)
