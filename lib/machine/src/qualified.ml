(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module C_spec = Act_compiler.Spec
module S_spec = Act_sim.Spec
module M_spec = Spec

module Compiler = struct
  type t = {c_spec: C_spec.With_id.t; m_spec: M_spec.With_id.t}
  [@@deriving make, fields, equal]

  module H = Act_utils.Inherit.Helpers (struct
    type nonrec t = t

    type c = C_spec.With_id.t

    let component = c_spec
  end)

  let style = H.forward C_spec.With_id.style

  let emits = H.forward C_spec.With_id.emits

  let cmd = H.forward C_spec.With_id.cmd

  let argv = H.forward C_spec.With_id.argv
end

module Sim = struct
  type t = {s_spec: S_spec.With_id.t; m_spec: M_spec.With_id.t}
  [@@deriving make, fields, equal]

  (* TODO(@MattWindsor91): properly chain here? *)
end

(** Basic signature used for constructing qualified lookups. *)
module type Basic_lookup = sig
  (** The unqualified inner specification module. *)
  module Inner : Act_common.Spec.S

  (** The qualified final specification type. *)
  type t

  val from_machine : Spec.t -> Inner.Set.t
  (** [from_machine] gets the list of unqualified specifications for the
      given machine. *)

  val qualify : Inner.With_id.t -> m_spec:Spec.With_id.t -> t
  (** [qualify spec ~m_spec] lifts an inner specification [spec] to a
      qualified spec with the machine being that described in [m_spec]. *)
end

module Make_lookup (B : Basic_lookup) :
  Qualified_types.S_lookup with type t = B.t = struct
  type t = B.t

  let lookup (machines : Spec.Set.t) ~(fqid : Act_common.Id.t) :
      t Or_error.t =
    Or_error.Let_syntax.(
      let%bind m_spec = Spec.Set.get_using_fqid machines ~fqid in
      let%bind inner_id =
        Act_common.Id.drop_prefix fqid ~prefix:(M_spec.With_id.id m_spec)
      in
      let specs = B.from_machine (Spec.With_id.spec m_spec) in
      let%map spec = B.Inner.Set.get specs inner_id in
      B.qualify spec ~m_spec)

  let all_on_machine (m_spec : Spec.With_id.t) : t list =
    m_spec |> Spec.With_id.spec |> B.from_machine
    |> B.Inner.Set.map ~f:(B.qualify ~m_spec)

  let all (machines : Spec.Set.t) : t list =
    List.concat (M_spec.Set.map ~f:all_on_machine machines)
end

module Lookup_compilers :
  Qualified_types.S_lookup with type t = Compiler.t = Make_lookup (struct
  type t = Compiler.t

  module Inner = Act_compiler.Spec

  let from_machine = Spec.compilers

  let qualify (c_spec : Act_compiler.Spec.With_id.t)
      ~(m_spec : Spec.With_id.t) =
    Compiler.make ~c_spec ~m_spec
end)

module Lookup_sims : Qualified_types.S_lookup with type t = Sim.t =
Make_lookup (struct
  type t = Sim.t

  module Inner = Act_sim.Spec

  let from_machine = Spec.sims

  let qualify (s_spec : Act_sim.Spec.With_id.t) ~(m_spec : Spec.With_id.t) =
    Sim.make ~s_spec ~m_spec
end)
