(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Generalised pretty-printer for Litmus tests

    This pretty-printer assumes that threads are laid out vertically, and is
    not suitable for assembly-style Litmus tests that lay out in tables.

    Specialisations of Litmus tests for specific languages will often provide
    a specialised form of {!pp}. *)

open Base

val pp : string -> 'k Fmt.t -> 't Fmt.t -> ('k, 't) Test.Raw.t Fmt.t
(** [pp langname ppk ppt] pretty-prints a raw Litmus test with language name
    [langname], constant printer [ppk], and thread printer [ppt]. *)
