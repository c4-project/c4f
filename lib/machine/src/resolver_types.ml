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

  val from_spec : spec -> (module Act_compiler.Instance_types.S) Or_error.t
  (** [from_spec spec] attempts to produce a first-class compiler module
      corresponding to [spec]. *)

  val filter_from_spec : spec -> (module Act_compiler.Filter.S) Or_error.t
  (** [filter_from_spec spec] attempts to produce a first-class compiler
      filter corresponding to [spec]. *)
end
