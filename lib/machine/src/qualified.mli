(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compiler and backend specifications that have been 'qualified' by
    attaching them to the spec of their parent machine. *)

open Base

(** {1 Basic shape of qualified specifications} *)

type 'qual t [@@deriving equal]
(** Opaque type of qualified specifications, parametrised over the type of
    the inner (non-machine) specification. *)

(** {2 Constructing a qualified specification} *)

val make :
  spec:'qual Act_common.Spec.With_id.t -> m_spec:Spec.With_id.t -> 'qual t
(** [make ~spec ~m_spec] wraps [spec] in a qualified specification over
    machine specification [m_spec]. *)

(** {2 Accessors}

    These can, of course, be used with {!Compiler.t} and {!Backend.t} *)

val spec : 'qual t -> 'qual Act_common.Spec.With_id.t
(** [spec q] unwraps [q], returning the original specification wrapped with
    its identifier. *)

val spec_without_id : 'qual t -> 'qual
(** [spec_without_id q] applies {!Act_common.Spec.With_id.spec} to [spec q]. *)

val m_spec : _ t -> Spec.t Act_common.Spec.With_id.t
(** [m_spec q] gets the machine spec within [q]. *)

(** {3 Identifiers} *)

val spec_id : _ t -> Act_common.Id.t
(** [spec_id q] gets the unqualified ID of the wrapped spec within [q]. *)

val m_spec_id : _ t -> Act_common.Id.t
(** [m_spec_id q] gets the unqualified ID of the machine spec within [q]. *)

val fqid : _ t -> Act_common.Id.t
(** [fqid q] gets the fully qualified ID (machine, dot, spec) of [q]. *)

(** {1 Qualified compiler specifications} *)
module Compiler : sig
  type nonrec t = Act_compiler.Spec.t t [@@deriving equal]
  (** Shorthand for a qualified compiler specification. *)

  (** {2 Resolving fully qualified IDs to compilers} *)

  val lift_resolver :
       t
    -> f:
         (   Act_compiler.Spec.With_id.t
          -> (module Act_compiler.Instance_types.Basic) Or_error.t)
    -> (module Act_compiler.Instance_types.S) Or_error.t
  (** [lift_resolver spec ~f] lifts the basic compiler resolving function [f]
      such that it accepts a qualified compiler spec [spec] and returns, on
      success, a {!Act_compiler.Instance_types.S} instance. *)
end

(** {1 Qualified backend specifications} *)
module Backend : sig
  type nonrec t = Act_backend.Spec.t t [@@deriving equal]
  (** Shorthand for a qualified backend specification. *)

  (** {2 Resolving fully qualified IDs to compilers} *)

  val lift_resolver :
       t
    -> f:
         (   Act_backend.Spec.With_id.t
          -> (   (module Act_backend.Runner_types.Basic)
              -> (module Act_backend.Runner_types.S))
             Or_error.t)
    -> (module Act_backend.Runner_types.S) Or_error.t
  (** [lift_resolver spec ~f] lifts the basic backend resolving function [f]
      such that it accepts a qualified backend spec [spec] and returns, on
      success, a {!Act_backend.Runner_types.S} instance. *)
end
