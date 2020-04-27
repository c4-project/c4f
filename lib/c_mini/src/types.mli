(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: module signatures and basic types *)

open Base

(** {1 General signatures} *)

(** Signature of modules that expose a 'named' part of a mini-model element,
    usually for compatibility with functors. *)
module type S_named = sig
  type elt

  type t = Act_common.C_id.t * elt [@@deriving equal]
end

(** Signature of abstract data types that wrap some C variable name. *)
module type S_has_underlying_variable = sig
  (** The type that contains underlying variables. *)
  type t

  val variable_of : t -> Act_common.C_id.t
  (** [variable_of x] is the underlying variable of [x]. *)
end

module type S_type_checker = sig
  (** The type being checked. *)
  type t

  val type_of : t -> Type.t Or_error.t
  (** [type_of x] tries to get the type of [x]. It fails if the type is
      inconsistent. *)
end

(** Signature of parts of the mini-model that implement type checking. *)
module type S_type_checkable = sig
  (** The type being checked. *)
  type t

  (** Type checking for [t]. *)
  module Type_check (E : Env_types.S) : S_type_checker with type t := t
end
