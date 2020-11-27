(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures used in {!Filter}. *)

open Stdio
open Base

(** Types and values common to both the basic and full filter signatures. *)
module type Common = sig
  (** Type of any auxiliary state consumed by this filter. *)
  type aux_i

  (** Type of any auxiliary state built by this filter. *)
  type aux_o

  val name : string
  (** [name] is the name of this filter. *)
end

module type Basic = sig
  include Common

  val run :
       aux_i Filter_context.t
    -> In_channel.t
    -> Out_channel.t
    -> aux_o Or_error.t
end

module type S = sig
  include Common

  val run : aux_i -> Input.t -> Output.t -> aux_o Or_error.t
  (** [run aux source sink] runs this filter on [source], outputs to [sink],
      reads and returns any auxiliary state on success. *)
end
