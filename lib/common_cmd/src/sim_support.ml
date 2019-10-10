(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts
module M_spec = Act_machine.Spec
module Sq_spec = Act_machine.Qualified.Sim

let sim_procs :
    ( Ac.Id.t
    ,    (module Act_backend.Runner_types.Basic)
      -> (module Act_backend.Runner_types.S) )
    List.Assoc.t =
  [ (Ac.Id.of_string "herd", Act_backend_herd.Runner.make)
  ; (Ac.Id.of_string "litmus", Act_backend_litmus.Runner.make) ]

let try_get_sim_proc (style_id : Ac.Id.t) :
    (   (module Act_backend.Runner_types.Basic)
     -> (module Act_backend.Runner_types.S))
    Or_error.t =
  Ac.Id.try_find_assoc_with_suggestions sim_procs style_id
    ~id_type:"sim style"

let try_make_sim (machine : M_spec.With_id.t) (spec : Act_backend.Spec.t) :
    (module Act_backend.Runner_types.S) Or_error.t =
  Or_error.Let_syntax.(
    let%map make_sim = try_get_sim_proc (Act_backend.Spec.style spec) in
    let (module R) = M_spec.With_id.runner machine in
    let module Basic : Act_backend.Runner_types.Basic = struct
      let spec = spec

      let machine_id = M_spec.With_id.id machine

      module Runner = R
    end in
    let (module Sim) = make_sim (module Basic) in
    (module Sim : Act_backend.Runner_types.S))

let make_error (e : Error.t) =
  ( module Act_backend.Runner.Make_error (struct
    let error = e
  end) : Act_backend.Runner_types.S )

let make_sim (machine : M_spec.With_id.t) (spec : Act_backend.Spec.t) :
    (module Act_backend.Runner_types.S) =
  match try_make_sim machine spec with Ok m -> m | Error e -> make_error e

module Make_resolver (B : sig
  val cfg : Act_config.Act.t
end) : Act_backend.Resolver_types.S = struct
  (* TODO(@MattWindsor91): use B.cfg to set up default simulators. *)

  let resolve_single (fqid : Act_common.Id.t) :
      (module Act_backend.Runner_types.S) Or_error.t =
    Or_error.Let_syntax.(
      let%map q_spec = Act_config.Act.sim B.cfg ~fqid in
      let m_spec = Sq_spec.m_spec q_spec in
      let s_spec = Sq_spec.s_spec q_spec in
      make_sim m_spec (Act_backend.Spec.With_id.spec s_spec))
end
