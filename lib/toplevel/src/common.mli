(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Glue code common to all top-level commands. *)

open Core_kernel
open Act_common

val asm_runner_of_target :
  Act_machine.Target.t -> (module Act_asm.Runner_intf.Basic) Or_error.t
(** [asm_runner_of_target target] gets the runner dependency module
    associated with a target (either a compiler spec or emits clause). *)

val lift_command :
     ?compiler_predicate:Act_compiler.Property.t Blang.t
  -> ?machine_predicate:Act_machine.Property.t Blang.t
  -> ?sanitiser_passes:Act_sanitiser.Pass_group.Selector.t Blang.t
  -> ?with_compiler_tests:bool (* default true *)
  -> f:(Args.Standard.t -> Output.t -> Act_config.Act.t -> unit Or_error.t)
  -> Args.Standard.t
  -> unit
(** [lift_command ?compiler_predicate ?machine_predicate ?sanitiser_passes
    ?with_compiler_tests ~f standard_args] lifts a command body [f],
    performing common book-keeping such as loading and testing the
    configuration, creating an [Output.t], and printing top-level errors. *)

val lift_command_with_files :
     ?compiler_predicate:Act_compiler.Property.t Blang.t
  -> ?machine_predicate:Act_machine.Property.t Blang.t
  -> ?sanitiser_passes:Act_sanitiser.Pass_group.Selector.t Blang.t
  -> ?with_compiler_tests:bool (* default true *)
  -> f:(   Args.Standard_with_files.t
        -> Output.t
        -> Act_config.Act.t
        -> unit Or_error.t)
  -> Args.Standard_with_files.t
  -> unit
(** [lift_command_with_files ?compiler_predicate ?machine_predicate
    ?sanitiser_passes ?with_compiler_tests ~f args] behaves like
    {{!lift_command} lift_command}, but also handles (and supplies) optional
    input and output files. *)

val lift_asm_command_basic :
     f:(   Args.Standard_asm.t
        -> Output.t
        -> Act_config.Act.t
        -> unit Or_error.t)
  -> Args.Standard_asm.t
  -> unit
(** [lift_asm_command_basic ~f args] behaves like
    {{!lift_command_with_files} lift_command_with_files}, but also handles
    (and supplies) the various standard asm command arguments, including
    sanitiser passes. More thorough lifting is left to the [Asm_common]
    module. *)
