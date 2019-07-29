(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compiler and sim specifications that have been 'qualified' by attaching
    them to the spec of their parent machine. *)

(** Bundles of compiler and machine specification. *)
module Compiler : sig
  include Qualified_types.S

  (** {3 Constructors} *)

  val make :
    c_spec:Act_compiler.Spec.With_id.t -> m_spec:Spec.With_id.t -> t

  (** {3 Accessors} *)

  val c_spec : t -> Act_compiler.Spec.With_id.t
  (** [c_spec spec] strips the machine spec from [spec], turning it into an
      {{!With_id.t} ordinary With_id.t}. *)

  (** {3 Using qualified compiler specifications as compiler specifications} *)

  include Act_compiler.Spec_types.S with type t := t
end

(** Bundles of simulator and machine specification. *)
module Sim : sig
  type t [@@deriving equal]
  (** Opaque type of machine-qualified sim specifications. *)

  (** {3 Constructors} *)

  val make : s_spec:Act_backend.Spec.With_id.t -> m_spec:Spec.With_id.t -> t

  (** {3 Accessors} *)

  val s_spec : t -> Act_backend.Spec.With_id.t
  (** [s_spec spec] strips the machine spec from [spec], turning it into an
      {{!Act_sim.Spec.With_id.t} ordinary Act_sim.Spec.With_id.t}. *)

  val m_spec : t -> Spec.With_id.t
  (** [m_spec spec] accesses the bundled machine specification inside
      [spec]. *)
end

(** {2 Implementing lookup} *)

module Lookup_compilers : Qualified_types.S_lookup with type t = Compiler.t
(** Implements fully-qualified ID lookup of compilers inside machine spec
    sets. *)

module Lookup_sims : Qualified_types.S_lookup with type t = Sim.t
(** Implements fully-qualified ID lookup of simulators inside machine spec
    sets. *)
