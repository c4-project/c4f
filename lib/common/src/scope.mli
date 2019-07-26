(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A lightweight notion of scope used in various bits of ACT. *)

(** Type of scopes. *)
type t = Local of int | Global [@@deriving compare, equal]

val is_global : t -> bool

val of_litmus_id : Litmus_id.t -> t
(** [of_litmus_id id] extracts the scope information from a Litmus ID. *)

val id_in_scope : t -> id:Litmus_id.t -> bool

val reduce : t * 'a -> t * 'a -> t * 'a
(** [reduce l r] returns [l] if the scope tag of [l] is local and [r] is
    global, [r] if vice versa, or an undefined choice of the two otherwise. *)
