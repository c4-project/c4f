(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Frontend onto {!Act_fir.Litmus_stats} using the Litmus/C frontend. *)

(** Filter from Litmus/C test files to statistic outputs. *)
module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit
