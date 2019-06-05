(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Machine specifications. *)

open Base

include Spec_types.S with type via := Via.t

val make :
     ?enabled:bool
  -> ?via:Via.t
  -> ?compilers:Act_compiler.Spec.Set.t
  -> ?sims:Act_sim.Spec.Set.t
  -> unit
  -> t
(** [make ?enabled ?via ?sims ()] creates a machine spec with the given
    fields.

    These fields are subject to change, and as such [make] is an unstable
    API. *)

(** [With_id] is an extension onto [Spec.With_id] that lets such items be
    machine references, and adds all of the [Spec] accessors. *)
module With_id : sig
  include Act_common.Spec_types.S_with_id with type elt := t

  include Spec_types.S with type t := t and type via := Via.t
end

(** Machine specifications are specifications. *)
include Act_common.Spec.S with type t := t and module With_id := With_id

(** We can monadically traverse the compiler specification sets in a machine
    specification. (Technically, there is only one, but phrasing it as a
    traversable helps us compose it to form {{!On_compilers} On_compilers}.) *)
module On_compiler_set :
  Travesty.Traversable.S0
  with type t = t
   and type Elt.t = Act_compiler.Spec.Set.t

(** We can monadically traverse the compiler specifications in a machine
    specification. *)
module On_compilers :
  Travesty.Traversable.S0
  with type t = t
   and type Elt.t = Act_compiler.Spec.t

(** Bundles of compiler and machine specification. *)
module Qualified_compiler : sig
  type t

  (** {3 Constructors} *)

  val make : c_spec:Act_compiler.Spec.With_id.t -> m_spec:With_id.t -> t

  (** {3 Accessors} *)

  val c_spec : t -> Act_compiler.Spec.With_id.t
  (** [c_spec spec] strips the machine spec from [spec], turning it into an
      {{!With_id.t} ordinary With_id.t}. *)

  val m_spec : t -> With_id.t
  (** [m_spec spec] accesses the bundled machine specification inside
      [spec]. *)

  include Act_compiler.Spec_types.S with type t := t
end

module Qualified_sim : sig
  type t [@@deriving equal]

  (** {3 Constructors} *)

  val make : s_spec:Act_sim.Spec.With_id.t -> m_spec:With_id.t -> t

  (** {3 Accessors} *)

  val s_spec : t -> Act_sim.Spec.With_id.t
  (** [s_spec spec] strips the machine spec from [spec], turning it into an
      {{!Act_sim.Spec.With_id.t} ordinary Act_sim.Spec.With_id.t}. *)

  val m_spec : t -> With_id.t
  (** [m_spec spec] accesses the bundled machine specification inside
      [spec]. *)
end
