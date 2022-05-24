(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Aux = struct
  module Output = struct
    type t = {trace: Fuzz.Trace.t; state: Fuzz.State.t}
  end

  module Randomised = struct
    type t =
      { seed: int option
      ; o: C4f_common.Output.t [@default C4f_common.Output.silent ()]
      ; config: Config.t [@main] }
    [@@deriving make]
  end

  module Replay = struct
    type t =
      { o: C4f_common.Output.t [@default C4f_common.Output.silent ()]
      ; trace: Fuzz.Trace.t [@main] }
    [@@deriving make]
  end
end

let run_on_litmus (test : Fir.Litmus.Test.t) ~(o : C4f_common.Output.t)
    ~(f : Fuzz.Subject.Test.t -> Fuzz.Output.t Fuzz.State.Monad.t) :
    (Fir.Litmus.Test.t * Aux.Output.t) Or_error.t =
  let subject = Fuzz.Subject.Test.of_litmus test in
  Or_error.Let_syntax.(
    let%bind state = Fuzz.State.of_litmus ~o test in
    let%bind state', {subject; trace} =
      Fuzz.State.Monad.run' (f subject) state
    in
    let%map test' = Fuzz.Subject.Test.to_litmus subject ~vars:state'.vars in
    (test', {Aux.Output.state= state'; trace}))

let run (input : Plumbing.Input.t) (output : Plumbing.Output.t)
    ~(o : C4f_common.Output.t)
    ~(f : Fuzz.Subject.Test.t -> Fuzz.Output.t Fuzz.State.Monad.t) :
    Aux.Output.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind test = Litmus_c.Frontend.Fir.load input in
    let%bind test', metadata = run_on_litmus ~o ~f test in
    let%map () =
      Utils.My_format.odump output (Fmt.vbox Litmus_c.Reify.pp_litmus) test'
    in
    metadata)

let run_randomised (input : Plumbing.Input.t) (output : Plumbing.Output.t)
    ~aux:({seed; o; config} : Aux.Randomised.t) : Aux.Output.t Or_error.t =
  run input output ~o ~f:(Randomised.run ?seed ~config)

let resolve_action (id : C4f_common.Id.t) : Fuzz.Action.t Or_error.t =
  Or_error.(
    id
    |> Utils.My_map.find_or_error ~sexp_of_key:C4f_common.Id.sexp_of_t
         ~map_name:"action map"
         (Lazy.force C4f_fuzz_actions.Table.action_map)
    >>| Fuzz.Action.With_default_weight.action)

let run_replay' (subject : Fuzz.Subject.Test.t) ~(trace : Fuzz.Trace.t) :
    Fuzz.Output.t Fuzz.State.Monad.t =
  Fuzz.State.Monad.(
    Let_syntax.(
      let%map subject =
        Fuzz.Trace.run trace subject ~resolve:resolve_action
      in
      {Fuzz.Output.subject; trace}))

let run_replay (input : Plumbing.Input.t) (output : Plumbing.Output.t)
    ~aux:({o; trace} : Aux.Replay.t) : Aux.Output.t Or_error.t =
  run input output ~o ~f:(run_replay' ~trace)
