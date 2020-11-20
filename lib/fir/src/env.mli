(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: variable environments

    Large parts of FIR's type checking and program fragment generation
    functionality depends on the existence of an environment mapping variable
    names to their {{!Type} FIR types}, and optionally {{!Constant} known
    constant values}. This module provides typing/known-value environments,
    modelled as maps. *)

open Base

(** {1 Environment records} *)

module Record : sig
  (** Opaque type of records. *)
  type t [@@deriving equal, sexp]

  val make : ?known_value:Constant.t -> type_of:Type.t -> unit -> t
  (** [make ?known_value ~type_of ()] creates a new record with the given
      type and optional known-value. *)

  (** {2 Accessors} *)

  val known_value :
    ('a, Constant.t, t, [< Accessor.optional]) Accessor.Simple.t
  (** [known_value] accesses a record's known value. *)

  val known_value_opt :
    ('a, Constant.t option, t, [< Accessor.field]) Accessor.Simple.t
  (** [known_value_opt] accesses a record's known value, but makes the
      optionality explicit. *)

  val type_of : ('a, Type.t, t, [< Accessor.field]) Accessor.Simple.t
  (** [type_of] accesses a record's type. *)
end

(** {1 Environment maps} *)

(** Full environments are maps of {!Record.t}. *)
type t = Record.t Map.M(Act_common.C_id).t [@@deriving sexp]

(** {2 Constructors} *)

val of_typing : Type.t Map.M(Act_common.C_id).t -> t
(** [of_typing] lifts a typing map to an environment. *)

val of_maps :
  Type.t Map.M(Act_common.C_id).t -> Constant.t Map.M(Act_common.C_id).t -> t
(** [of_maps] combines a typing map and known-value map. *)

(** {3 Lookup} *)

val record_of : t -> id:Act_common.C_id.t -> Record.t Or_error.t
(** [record_of env ~id] gets the environment record in [env] for [id]. *)

val type_of : t -> id:Act_common.C_id.t -> Type.t Or_error.t
(** [type_of env ~id] gets the type in [env] for [id]. *)

val known_value : t -> id:Act_common.C_id.t -> Constant.t option Or_error.t
(** [known_value env ~id] gets the known value in [env] for [id], if any. *)

val type_of_known_value : t -> id:Act_common.C_id.t -> Type.t Or_error.t
(** [type_of_known_value env ~id] behaves like [type_of], but returns the
    type associated with any known-value information stored for [id]. This is
    [type_of id] for non-pointer-typed variables, and the non-pointer
    equivalent otherwise. *)

(** {2 Filtering and extracting} *)

val typing : t -> Type.t Map.M(Act_common.C_id).t
(** [typing env] creates a map from variable IDs to their types. *)

val variables_with_known_values :
  t -> (Type.t * Constant.t) Map.M(Act_common.C_id).t
(** [variables_with_known_values env] creates a map from variable IDs to
    their types and definitely-known values, discarding any variable IDs for
    which there is no known value. *)

val filter_to_known_values : t -> t
(** [filter_to_known_values env] behaves like [variables_with_known_values],
    but returns another environment. *)

val has_vars_of_prim_type : t -> prim:Type.Prim.t -> bool
(** [has_vars_of_prim_type env ~prim] is true provided that [env] has at
    least one variable whose primitive type is [prim]. *)

val variables_of_prim_type : t -> prim:Type.Prim.t -> t
(** [variables_of_prim_type env ~prim] filters [env], returning a map binding
    only variables whose prim type is [t]. *)

val has_vars_of_basic_type : t -> basic:Type.Basic.t -> bool
(** [has_vars_of_basic_type env ~basic] is true provided that [env] has at
    least one variable whose basic type is [basic]. *)

val variables_of_basic_type : t -> basic:Type.Basic.t -> t
(** [variables_of_basic_type env ~basic] filters [env], returning a map
    binding only variables whose basic type is [t]. *)

(** {2 Quickcheck-compatible random variable selection} *)

val gen_random_var : t -> Act_common.C_id.t Base_quickcheck.Generator.t
(** [gen_random_var env] is a generator that selects a random variable name
    from [env]. *)

val gen_random_var_with_type :
  t -> Type.t Act_common.C_named.t Base_quickcheck.Generator.t
(** [gen_random_var_with_type env] is a generator that selects a random
    variable typing record from [env]. *)

val gen_random_var_with_record :
  t -> Record.t Act_common.C_named.t Base_quickcheck.Generator.t
(** [gen_random_var_with_record env] is a generator that selects a random
    variable record from [env]. *)

(** {3 Modules}

    These wrap the above generators in Quickcheck-compatible modules. *)

(** Selection of random variables from an environment. *)
module Random_var (E : sig
  val env : t
end) : sig
  type t = Act_common.C_id.t [@@deriving sexp_of, quickcheck]
end

(** Selection of random variables, and their types, from an environment. *)
module Random_var_with_type (E : sig
  val env : t
end) : sig
  type t = Type.t Act_common.C_named.t [@@deriving sexp_of, quickcheck]
end

(** Selection of random variables, and their records, from an environment. *)
module Random_var_with_record (E : sig
  val env : t
end) : sig
  type t = Record.t Act_common.C_named.t [@@deriving sexp_of, quickcheck]
end
