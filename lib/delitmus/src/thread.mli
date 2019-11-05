(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmusifier: thread contexts as modules. *)

open Base

(** Module type of modularised thread contexts. *)
module type S = sig
  val tid : int
  (** [tid] is the thread ID of the thread. *)

  val when_local :
       'a
    -> over:('a -> Act_common.C_id.t)
    -> f:('a -> 'a Or_error.t)
    -> 'a Or_error.t
  (** [when_local x ~over ~f] returns [f x] when [over x] is local, and [x]
      otherwise. *)

  val when_global :
       'a
    -> over:('a -> Act_common.C_id.t)
    -> f:('a -> 'a Or_error.t)
    -> 'a Or_error.t
  (** [when_local x ~over ~f] returns [x] when [over x] is local, and [f x]
      otherwise. *)
end

(** Makes a thread context module from a thread ID and local environment. *)
module Make (B : sig
  val tid : int

  val locals : Set.M(Act_common.C_id).t
end) : S
