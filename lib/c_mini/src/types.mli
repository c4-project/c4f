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
  type t
  (** The type that contains underlying variables. *)

  val variable_of : t -> Act_common.C_id.t
  (** [variable_of x] is the underlying variable of [x]. *)

  val variable_in_env : t -> env:_ Map.M(Act_common.C_id).t -> bool
  (** [variable_in_env x ~env] gets whether [x]'s underlying variable name is
      a key in an environment [env]. *)
end

(** Signature of parts of the mini-model that implement type checking. *)
module type S_type_checkable = sig
  type t
  (** The type being checked. *)

  module Type_check (E : Env_types.S) : sig
    val type_of : t -> Type.t Or_error.t
    (** [type_of x] tries to get the type of [x] given the variable typing
        environment [E.env]. It fails if the type is inconsistent. *)
  end
end

(** {2 Signatures for recursive modules} *)

module type S_address_traversable = sig
  type t

  type address

  (** Traversing over atomic-action addresses. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = address
end

module type S_lvalue_traversable = sig
  type t

  type lvalue

  (** Traversing over lvalues. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = lvalue
end

(** Signature of c-mini modules that facilitate traversing over all of the
    typical features of a c-mini node, so long as any metadata has been
    fixed. *)
module type S_with_meta = sig
  type 'meta t

  type address

  type lvalue

  type identifier

  module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t
  (** We can traverse over the metadata. *)

  val erase_meta : 'meta t -> unit t
  (** [erase_meta x] deletes all of [x]'s metadata. *)

  (** By fixing the metadata type, we can perform various forms of standard
      traversal. *)
  module With_meta (Meta : T) : sig
    type nonrec t = Meta.t t

    include
      S_address_traversable with type t := t and type address := address

    include S_lvalue_traversable with type t := t and type lvalue := lvalue
  end
end
