(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

(* This M brought to you by ppx_deriving not supporting nonrec *)
module M = struct
  type 'qual t =
    {spec: 'qual Act_common.Spec.With_id.t; m_spec: Spec.With_id.t}
  [@@deriving make, fields, equal]
end

include M

module On_specs = Travesty.Bi_traversable.Make1_left (struct
  type nonrec 'qual t = 'qual t

  type right = Spec.t

  module On_monad (M : Monad.S) = struct
    module With_id = Act_common.Spec.With_id.On_monad (M)

    let bi_map_m (type q1 q2) (qspec : q1 t) ~(left : q1 -> q2 M.t)
        ~(right : right -> right M.t) : q2 t M.t =
      M.Let_syntax.(
        let%map spec = With_id.map_m ~f:left (spec qspec)
        and m_spec = With_id.map_m ~f:right (m_spec qspec) in
        make ~spec ~m_spec)
  end
end)

let m_spec_id (q : _ t) : Ac.Id.t = Ac.Spec.With_id.id (M.m_spec q)

let spec_id (q : _ t) : Ac.Id.t = Ac.Spec.With_id.id (M.spec q)

let fqid (q : _ t) : Ac.Id.t = Ac.Id.(m_spec_id q @. spec_id q)

let spec_without_id (q : 'qual t) : 'qual =
  q |> spec |> Act_common.Spec.With_id.spec

module Compiler = struct
  type t = Act_compiler.Spec.t M.t [@@deriving equal]

  let lift_resolver (q_spec : t)
      ~(f :
            Act_compiler.Spec.With_id.t
         -> (module Act_compiler.Instance_types.Basic) Or_error.t) :
      (module Act_compiler.Instance_types.S) Or_error.t =
    let c_spec = spec q_spec in
    let m_spec = m_spec q_spec in
    let (module Runner) = Spec.With_id.runner m_spec in
    Or_error.Let_syntax.(
      let%map (module B : Act_compiler.Instance_types.Basic) = f c_spec in
      ( module Act_compiler.Instance.Make (struct
        let spec = c_spec

        include B
        module Runner = Runner
      end) : Act_compiler.Instance_types.S ))
end

module Backend = struct
  type t = Act_backend.Spec.t M.t [@@deriving equal]

  let lift_resolver (q_spec : t)
      ~(f :
            Act_backend.Spec.With_id.t
         -> (module Act_backend.Instance_types.Basic) Or_error.t) :
      (module Act_backend.Instance_types.S) Or_error.t =
    let s_spec = spec q_spec in
    let m_spec = m_spec q_spec in
    let (module Runner) = Spec.With_id.runner m_spec in
    Or_error.Let_syntax.(
      let%map (module B : Act_backend.Instance_types.Basic) = f s_spec in
      ( module Act_backend.Instance.Make (struct
        let spec = s_spec

        include B
        module Runner = Runner
      end) : Act_backend.Instance_types.S ))
end
