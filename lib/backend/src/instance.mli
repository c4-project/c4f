(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Backend instances.

    This module, and its {!Instance_types} counterpart, provide a standard
    interface for interacting with C and assembly backends, such as Herd and
    Litmus, as well as a functor for implementing that interface using a
    filter wrapper over the backend and a backend output parser. *)

(** [Make] makes a backend instance given a combination of a skeleton
    instance (without machine or runner information) and the running context. *)
module Make (B : Instance_types.Basic_with_run_info) : Instance_types.S

(** {1 Helpers} *)

val no_make_harness : Spec.t -> arch:Arch.t -> Capability.Make_harness.t
(** [no_make_harness] is a dummy value for [make_harness] that raises an
    error stating that the backend doesn't support it. *)
