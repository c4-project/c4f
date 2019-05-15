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

let litmus_config (machine : Act_config.Machine.Spec.With_id.t) :
    Act_config.Litmus_tool.t Or_error.t =
  Or_error.tag_arg
    (Tx.Option.one (Act_config.Machine.Spec.With_id.litmus machine))
    "While trying to find litmus config for machine"
    (Act_config.Machine.Spec.With_id.id machine)
    [%sexp_of: Act_config.Machine.Id.t]

let try_make_litmus_filter (machine : Act_config.Machine.Spec.With_id.t) :
    (module Act_sim.Runner.S) Or_error.t =
  Or_error.Let_syntax.(
    let%map litmus_cfg = litmus_config machine in
    let (module R) = Act_config.Machine.Spec.With_id.runner machine in
    ( module Act_sim_litmus.Runner.Make (struct
      let config = litmus_cfg

      let machine_id = Act_config.Machine.Spec.With_id.id machine

      module Runner = R
    end)
    : Act_sim.Runner.S ))

let make_error (e : Error.t) =
  ( module Act_sim.Runner.Make_error (struct
    let error = e
  end)
  : Act_sim.Runner.S )

let make_litmus_filter (machine : Act_config.Machine.Spec.With_id.t) :
    (module Act_sim.Runner.S) =
  match try_make_litmus_filter machine with
  | Ok m ->
      m
  | Error e ->
      make_error e

let make_herd_filter (cfg : Act_config.Act.t) : (module Act_sim.Runner.S) =
  let cfg = Act_config.Act.herd_or_default cfg in
  ( module Act_sim_herd.Runner.Make (struct
    let config = cfg
  end) )

let make_simulator_table (cfg : Act_config.Act.t)
    (machine : Act_config.Machine.Spec.With_id.t) :
    Act_sim.Table.t Or_error.t =
  Act_sim.Table.make
    [ (Id.of_string "herd", make_herd_filter cfg)
    ; (Id.of_string "litmus", make_litmus_filter machine) ]

module Make_resolver (B : sig
  val cfg : Act_config.Act.t
end) : Act_sim.Resolver.S = struct
  let get_machine :
      Act_common.Id.t -> Act_config.Machine.Spec.With_id.t Or_error.t =
    Act_config.Machine.Spec.Set.get (Act_config.Act.machines B.cfg)

  let make_table (id : Act_common.Id.t) : Act_sim.Table.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind machine = get_machine id in
      make_simulator_table B.cfg machine)
end
