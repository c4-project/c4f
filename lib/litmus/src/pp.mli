(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
