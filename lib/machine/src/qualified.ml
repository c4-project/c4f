(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts
module C_spec = Act_compiler.Spec
module S_spec = Act_backend.Spec
module M_spec = Spec

module Compiler = struct
  module M = struct
    type t = {c_spec: C_spec.With_id.t; m_spec: M_spec.With_id.t}
    [@@deriving make, fields, equal]
  end

  include M

  include C_spec.Forward_spec (M) (C_spec.With_id)
            (struct
              let component = c_spec
            end)

  let lift_resolver (q_spec : t)
      ~(f :
            Act_compiler.Spec.With_id.t
         -> (module Act_compiler.Instance_types.Basic) Or_error.t) :
      (module Act_compiler.Instance_types.S) Or_error.t =
    let c_spec = c_spec q_spec in
    let m_spec = m_spec q_spec in
    Or_error.Let_syntax.(
      let%map (module B : Act_compiler.Instance_types.Basic) = f c_spec in
      let (module Runner) = Spec.With_id.runner m_spec in
      ( module Act_compiler.Instance.Make (struct
        let cspec = c_spec

        include B
        module Runner = Runner
      end) : Act_compiler.Instance_types.S ))
end

module Sim = struct
  type t = {s_spec: S_spec.With_id.t; m_spec: M_spec.With_id.t}
  [@@deriving make, fields, equal]

  let lift_resolver (q_spec : t)
      ~(f :
            Act_backend.Spec.With_id.t
         -> (   (module Act_backend.Runner_types.Basic)
             -> (module Act_backend.Runner_types.S))
            Or_error.t) : (module Act_backend.Runner_types.S) Or_error.t =
    let s_spec = s_spec q_spec in
    let m_spec = m_spec q_spec in
    Or_error.Let_syntax.(
      let%map backend_maker = f s_spec in
      let (module Runner) = Spec.With_id.runner m_spec in
      let (b : (module Act_backend.Runner_types.Basic)) =
        ( module struct
          let spec = Ac.Spec.With_id.spec s_spec

          let machine_id = Ac.Spec.With_id.id m_spec

          module Runner = Runner
        end )
      in
      backend_maker b)
end
