(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functor for making the bits of {!Input} and {!Output} that are common to
    both.

    Client code shouldn't usually use this. *)

(** [Make] makes the IO code common to {!Input} and {!Output}. *)
module Make (B : sig
  type t

  val of_fpath : Fpath.t -> t

  val to_fpath_opt : t -> Fpath.t option

  val std : unit -> t

  val std_name : string
end) : Io_types.Common with type t := B.t
