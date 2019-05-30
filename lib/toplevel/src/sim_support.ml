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
open Act_common
module Tx = Travesty_base_exts
module M_spec = Act_compiler.Machine.Spec

let litmus_config (machine : M_spec.With_id.t) :
    Act_compiler.Litmus_tool.t Or_error.t =
  Or_error.tag_arg
    (Tx.Option.one (M_spec.With_id.litmus machine))
    "While trying to find litmus config for machine"
    (M_spec.With_id.id machine)
    [%sexp_of: Id.t]

let try_make_litmus_filter (machine : M_spec.With_id.t) :
    (module Act_sim.Runner_intf.S) Or_error.t =
  Or_error.Let_syntax.(
    let%map litmus_cfg = litmus_config machine in
    let (module R) = M_spec.With_id.runner machine in
    ( module Act_sim_litmus.Runner.Make (struct
      let config = litmus_cfg

      let machine_id = M_spec.With_id.id machine

      module Runner = R
    end)
    : Act_sim.Runner_intf.S ))

let make_error (e : Error.t) =
  ( module Act_sim.Runner.Make_error (struct
    let error = e
  end)
  : Act_sim.Runner_intf.S )

let make_litmus_filter (machine : M_spec.With_id.t) :
    (module Act_sim.Runner_intf.S) =
  match try_make_litmus_filter machine with
  | Ok m ->
      m
  | Error e ->
      make_error e

let make_herd_filter (cfg : Act_config.Act.t) :
    (module Act_sim.Runner_intf.S) =
  let cfg = Act_config.Act.herd_or_default cfg in
  ( module Act_sim_herd.Runner.Make (struct
    let config = cfg
  end) )

let make_simulator_table (cfg : Act_config.Act.t)
    (machine : M_spec.With_id.t) : Act_sim.Table.t Or_error.t =
  Act_sim.Table.make
    [ (Id.of_string "herd", make_herd_filter cfg)
    ; (Id.of_string "litmus", make_litmus_filter machine) ]

module Make_resolver (B : sig
  val cfg : Act_config.Act.t
end) : Act_sim.Resolver.S = struct
  let get_machine : Act_common.Id.t -> M_spec.With_id.t Or_error.t =
    M_spec.Set.get (Act_config.Act.machines B.cfg)

  let make_table (id : Act_common.Id.t) : Act_sim.Table.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind machine = get_machine id in
      make_simulator_table B.cfg machine)
end
