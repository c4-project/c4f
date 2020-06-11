(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer utilities for manipulating labels. *)

open Base

val labels_of_test : Act_fir.Litmus.Test.t -> Set.M(Act_common.Litmus_id).t
(** [labels_of_test test] extracts the labels from [test]. *)

val gen_fresh :
     Set.M(Act_common.Litmus_id).t
  -> Act_common.C_id.t Base_quickcheck.Generator.t
(** [gen_fresh set] generates random labels that don't shadow existing labels
    (regardless of thread ID) in [set]. *)
