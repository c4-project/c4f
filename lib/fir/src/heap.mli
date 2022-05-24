(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A basic model of a heap, used for effectful expression evaluation.

    This heap model consists of a 'source' function that models the initial
    state of the heap, and additional information about edits made to the
    heap thereafter.

    The heap model has a few restrictions:

    - it only models sequentially consistent behaviour;
    - it assumes that no addresses are aliased; it considers two addresses
      equal if and only if they normalise to the same address. *)

open Base

(** Opaque type of heaps. *)
type t

(** {1 Constructors} *)

val empty : unit -> t
(** [empty] constructs an empty heap. *)

val make : (Address.t -> Constant.t Or_error.t) -> t
(** [make source] makes a heap with an initial source. *)

(** {1 Memory actions} *)

val load : t -> address:Address.t -> Constant.t Or_error.t
(** [load heap ~address] loads [address] from [heap], failing if neither the
    source nor updates made afterwards can locate it. *)

val store : t -> address:Address.t -> data:Constant.t -> t
(** [store heap ~address ~data] stores [data] to [address] in [heap],
    returning the updated heap. *)

(** {1 State monad} *)

module Monad : sig
  include
    Travesty.State_transform_types.S
      with type state := t
       and type 'a Inner.t = 'a Or_error.t

  (** {2 Stateful memory actions} *)

  val load : Address.t -> Constant.t t
  (** [load ~address] is a stateful action that loads [address] from the
      current heap. *)

  val store : Address.t -> Constant.t -> unit t
  (** [store heap ~address] is a stateful action that stores [data] to
      [address] in the current heap. *)
end
