(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compiler resolver logic. *)

(** {2 Signatures with fixed types} *)

(** Version of {{!Resolver_types.Basic} Resolver_types.Basic} with the
    specification type fixed. *)
module type Basic =
  Resolver_types.Basic with type spec := Act_compiler.Spec.With_id.t

(** Version of {{!Resolver_types.S} Resolver_types.S} with the chain input
    fixed. *)
module type S =
  Resolver_types.S
  with type 'a chain_input := 'a Act_compiler.Filter.Chain_input.t

(** {2 Resolving spec IDs to compilers} *)

(** Constructs a {{!S_resolver} S_resolver} from a
    {{!Basic_resolver} Basic_resolver} over direct compiler specs. *)
module Make (B : Basic) : S with type spec = Qualified.Compiler.t

(** Constructs a {{!S_resolver} S_resolver} over targets from a
    {{!Basic_resolver} Basic_resolver}. *)
module Make_on_target (B : Basic) : S with type spec = Target.t
