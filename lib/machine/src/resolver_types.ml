(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Resolving spec IDs to compilers.

    These signatures describe modules that allow one to retrieve
    (first-class) compiler modules at run-time from compiler specifications. *)

module type Basic = sig
  (** Type of specifications consumed by this resolver. *)
  type spec

  val resolve :
    spec -> (module Act_compiler.Instance_types.Basic) Or_error.t
end

(** Signature of fully-instantiated resolvers. *)
module type S = sig
  (** Type of specifications consumed by this resolver. *)
  type spec

  (** Type of chained-filter input. *)
  type 'a chain_input

  val from_spec : spec -> (module Act_compiler.Instance_types.S) Or_error.t
  (** [from_spec spec] attempts to produce a first-class compiler module
      corresponding to [spec]. *)

  val filter_from_spec :
       spec
    -> (module Plumbing.Filter_types.S
          with type aux_i = unit
           and type aux_o = unit)
       Or_error.t
  (** [filter_from_spec spec] attempts to produce a first-class compiler
      filter corresponding to [spec]. *)

  val chained_filter_from_spec :
       spec
    -> (module Plumbing.Filter_types.S
          with type aux_i = 'i
           and type aux_o = 'o)
    -> (module Plumbing.Filter_types.S
          with type aux_i = 'i chain_input
           and type aux_o = 'o)
       Or_error.t
end
