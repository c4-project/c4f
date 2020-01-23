(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer-subject labels and tools for manipulating them.

    This module mainly exists to provide a specialised version of
    {!Act_c_mini.Label} over {!Metadata}, but it also includes adjacent
    functionality. *)

(** {1 Specialised label type} *)

open Base

type t = Metadata.t Act_c_mini.Label.t [@@deriving compare, equal, sexp]
(** [t] is an alias for {!Metadata.t} {!Act_c_mini.Label.t}. *)

include Comparable.S with type t := t

(** {1 Probing Litmus tests for labels} *)

val labels_of_test :
  Act_c_mini.Litmus.Test.t -> (t, comparator_witness) Set.t
(** [labels_of_test test] extracts the labels from [test]. *)
