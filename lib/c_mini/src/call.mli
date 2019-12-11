(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Mini-C: function calls

    Note that many things that appear in a C AST as function calls actually
    map to primitives: for example, atomic stores become {!Atomic_store.t}. *)

type 'meta t [@@deriving sexp, equal]
(** Opaque type of function calls. *)

(** {1 Constructors} *)

val make :
     ?arguments: Expression.t list
  -> metadata: 'meta
  -> function_id: Act_common.C_id.t
  -> unit
  -> 'meta t
(** [make ?arguments ~metadata ~function_id ()] constructs a function call with the
    metadata [metadata], function ID [function_id], and arguments [arguments]. *)

(** {1 Accessors} *)

val metadata: 'meta t -> 'meta
(** [metadata call] gets the metadata associated with [call]. *)

val function_id: _ t -> Act_common.C_id.t
(** [function_id call] gets the identifier of the function called by [call]. *)

val arguments: _ t -> Expression.t list
(** [arguments call] gets the argument expressions of [call]. *)

(** {1 Traversals} *)

module On_meta : Travesty.Traversable_types.S1 with type 'meta t = 'meta t
(** [On_meta] traverses across the metadata in a function call. *)

(** Traversals that require a fixed metadata type argument. *)
module With_meta (Meta : T) : sig
  (** Traverses over the lvalues of a function call. *)
  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t

  (** Traverses over the addresses of a function call. *)
  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t

  (** Traverses over the identifiers of a function call. *)
  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Act_common.C_id.t
end
