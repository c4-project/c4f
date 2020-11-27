(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Frontend onto {!Act_fir.Litmus_stats} using the Litmus/C frontend. *)

val run : Plumbing.Input.t -> Plumbing.Output.t -> unit Base.Or_error.t
(** [run input output] reads in a Litmus/C test from [input], scrapes its
    statistics, and dumps the statset to [output]. *)
