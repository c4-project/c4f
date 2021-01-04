(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Helper functions for dealing with various patterns of {{!Input} Input}
    and {{!Output} Output} usage. *)

open Base
open Stdio

val with_input_and_output :
     Input.t
  -> Output.t
  -> f:(In_channel.t -> Out_channel.t -> 'a Or_error.t)
  -> 'a Or_error.t
(** [with_input_and_output i o ~f] runs [f] with the appropriate channels
    pointed to by [i] and [o]. *)

val lift_to_raw_strings :
     f:('i -> Input.t -> Output.t -> 'o Or_error.t)
  -> 'i
  -> infile:string option
  -> outfile:string option
  -> 'o Or_error.t

val lift_to_fpaths :
     f:('i -> Input.t -> Output.t -> 'o Or_error.t)
  -> 'i
  -> infile:Fpath.t option
  -> outfile:Fpath.t option
  -> 'o Or_error.t
