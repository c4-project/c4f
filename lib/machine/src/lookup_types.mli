(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for resolvers. *)

open Core_kernel

(** General template for resolver types.

    You'll want {!S_compiler} or {!S_backend} instead. *)
module type S = sig
  type spec

  type property

  val filtered_list :
       ?machine_predicate:Property.t Blang.t
    -> ?predicate:property Blang.t
    -> ?test_specs:bool
    -> Spec.Set.t
    -> spec Lookup_listing.t Or_error.t
  (** [filtered_list ?machine_predicate ?predicate ?test_specs specs]
      produces a filtered list of compiler or backend specs from the
      configured spec set [specs].

      If [test_specs] is set to true, the filtering process may {!resolve}
      each spec to a compiler or backend, then perform a quick test to make
      sure it works. *)

  val lookup_single :
       ?default_machines:Act_common.Id.t list
    -> ?machine_predicate:Property.t Blang.t
    -> ?predicate:property Blang.t
    -> ?test_specs:bool
    -> Spec.Set.t
    -> fqid:Act_common.Id.t
    -> spec Or_error.t
  (** [lookup_single ?default_machines ?machine_predicate ?predicate
      ?test_specs spec ~fqid] is akin to using {!filtered_list} then
      {!Lookup_listing.lookup} with the appropriate arguments, but may be
      faster in future versions of ACT. *)
end

(** [S_compiler] is the module type of compiler resolvers: resolvers that
    consume {!Qualified.Compiler.t} specs and emit
    {!Act_compiler.Instance_types.S} modules. *)
module type S_compiler =
  S
    with type spec := Qualified.Compiler.t
     and type property := Act_compiler.Property.t

(** [S_backend] is the module type of backend resolvers: resolvers that
    consume {!Qualified.Backend.t} specs and emit
    {!Act_backend.Runner_types.S} modules. *)
module type S_backend =
  S with type spec := Qualified.Backend.t and type property := unit

(* for now *)
