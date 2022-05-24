(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
