(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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
