(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer utilities for manipulating labels. *)

open Base

val labels_of_test : C4f_fir.Litmus.Test.t -> Set.M(C4f_common.Litmus_id).t
(** [labels_of_test test] extracts the labels from [test]. *)

val gen_fresh :
     Set.M(C4f_common.Litmus_id).t
  -> C4f_common.C_id.t Base_quickcheck.Generator.t
(** [gen_fresh set] generates random labels that don't shadow existing labels
    (regardless of thread ID) in [set]. *)
