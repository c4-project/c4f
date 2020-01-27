(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer utilities for manipulating labels. *)

(** {1 Specialised label type} *)

open Base

val labels_of_test :
  Act_c_mini.Litmus.Test.t -> Set.M(Act_common.Litmus_id).t
(** [labels_of_test test] extracts the labels from [test]. *)
