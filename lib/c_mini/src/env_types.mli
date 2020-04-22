(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: module signatures for variable typing environments *)

open Base

(** Extended signature of environment modules. *)
module type S = sig
  val env : Type.t Map.M(Act_common.C_id).t
  (** [env] is a variable typing environment. *)

  (** [Random_var] allows generation of random variables from the variable
      environment. *)
  module Random_var : sig
    type t = Act_common.C_id.t [@@deriving sexp_of, quickcheck]
  end

  val type_of : Act_common.C_id.t -> Type.t Or_error.t
  (** [type of id] tries to get the type of [id] in the typing environment. *)

  val has_variables_of_basic_type : Type.Basic.t -> bool
  (** [has_variables_of_basic_type t] is true provided that the environment
      has at least one variable whose basic type is [t]. *)

  val variables_of_basic_type :
    Type.Basic.t -> Type.t Map.M(Act_common.C_id).t
  (** [atomic_int_variables t] filters the environment, returning a map
      binding only variables whose basic type is [t]. *)
end

(** {2 Known values}

    Some generators require information about the current known values of a
    set of variables. These signatures extend the idea of a typing
    environment with a known-values map. *)

module type S_with_known_values = sig
  include S

  val known_value : Act_common.C_id.t -> Constant.t option
  (** [known_value var] gets the known value of [var], if any. *)

  val variables_with_known_values :
    (Type.t * Constant.t) Map.M(Act_common.C_id).t Lazy.t
  (** [variables_with_known_values] creates a map from variable IDs to their
      types and definitely-known values, discarding any variable IDs for
      which there is no known value. *)

  val type_of_known_value : Act_common.C_id.t -> Type.t Or_error.t
  (** [type_of_known_value id] behaves like [type_of id], but returns the
      type associated with any known-value information stored for [id]. This
      is [type_of id] for non-pointer-typed variables, and the non-pointer
      equivalent otherwise. *)

  val known_value_env : (module S) Lazy.t
  (** [known_value_env] is a lazy computation that produces a typing map only
      containing the values in this environment for which there exist known
      values. *)

end
