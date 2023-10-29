(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: module signatures and basic types *)

(* Needed because Base shadows it: *)
module Ty = Type

open Base
open Import

(** {1 General signatures} *)

(** Signature of modules that expose a 'named' part of a FIR element, usually
    for compatibility with functors. *)
module type S_named = sig
  type elt

  type t = C4f_common.C_id.t * elt [@@deriving equal]
end

(** Signature of abstract data types that wrap some C variable name. *)
module type S_has_underlying_variable = sig
  (** The type that contains underlying variables. *)
  type t

  val variable_of : ('i, Common.C_id.t, t, [< field]) Accessor.t
  (** [variable_of] accesses this object's underlying variable. *)
end

module type S_type_checker = sig
  (** The type being checked. *)
  type t

  val type_of : t -> Ty.t Or_error.t
  (** [type_of x] tries to get the type of [x]. It fails if the type is
      inconsistent. *)
end

(** Signature of parts of FIR that implement type checking. *)
module type S_type_checkable = sig
  (** The type being checked. *)
  type t

  (** Type checking for [t]. *)
  module Type_check (_ : Env_types.S) : S_type_checker with type t := t
end
