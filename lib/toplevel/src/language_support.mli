(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Language lookup and module building support

    The various top-level act commands need to invoke the
    language-independent bits of act (in the [Lib] module) using the
    appropriate language-dependent bits. This module works out which
    language is needed by looking at the 'emits' clause of a compiler spec,
    and hooks up the correct language-dependent modules. *)

open Core_kernel
open Act_common

val asm_runner_of_arch :
  Id.t -> (module Act_asm.Runner_intf.Basic) Or_error.t
(** [asm_runner_of_arch arch] generates an assembly job runner from an
    architecture ID [arch]. *)

val asm_runner_of_target :
  Act_machine.Qualified.Compiler.t Act_machine.Target.t -> (module Act_asm.Runner_intf.Basic) Or_error.t
(** [asm_runner_of_target target] gets the runner dependency module
    associated with a target (either a compiler spec or emits clause). *)

module Resolve_compiler :
  Act_machine.Resolver_types.S
    with type spec = Act_machine.Qualified.Compiler.t
(** Compiler resolver that uses this module's built-in compiler table to
    look up compilers. *)

module Resolve_compiler_from_target :
  Act_machine.Resolver_types.S with type spec = Act_machine.Qualified.Compiler.t Act_machine.Target.t
(** Compiler resolver that uses this module's built-in compiler table to
    look up compilers from targets, filling in a dummy compiler if the
    target doesn't mention a compiler. *)

val make_filtered_machine_config :
     ?compiler_predicate:Act_compiler.Property.t Blang.t
  -> ?machine_predicate:Act_machine.Property.t Blang.t
  -> ?with_compiler_tests:bool (* default true *)
  -> Act_config.Global.t
  -> Act_config.Act.t Or_error.t
(** [make_filtered_machine_config ?compiler_predicate ?machine_predicate
    ?with_compiler_tests config] uses the config [global], as well as the
    given compiler and machine filtering predicates, to produce a filtered
    machine configuration.

    If [compiler_predicate] (a Blang expression) is present, only compilers
    satisfying it (on enabled machines) will be accepted.

    If [machine_predicate] (also a Blang expression) is present, only
    machines satisfying it will be enabled.

    If [with_compiler_tests] is present and true, compilers will be tested
    for reachability. *)
