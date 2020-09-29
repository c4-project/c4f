(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import
open Stdio

module Aux = struct
  module Randomised = struct
    type t =
      { seed: int option
      ; o: Act_common.Output.t [@default Act_common.Output.silent ()]
      ; config: Config.t [@main] }
    [@@deriving make]
  end

  module Replay = struct
    type t =
      { o: Act_common.Output.t [@default Act_common.Output.silent ()]
      ; trace: Fuzz.Trace.t [@main] }
    [@@deriving make]
  end
end

let run_on_litmus (test : Fir.Litmus.Test.t) ~(o : Act_common.Output.t)
    ~(f : Fuzz.Subject.Test.t -> 'm Fuzz.Output.t Fuzz.State.Monad.t) :
    (Fir.Litmus.Test.t * 'm) Or_error.t =
  let subject = Fuzz.Subject.Test.of_litmus test in
  Or_error.Let_syntax.(
    let%bind state = Fuzz.State.of_litmus ~o test in
    let%bind {vars; _}, output = Fuzz.State.Monad.run' (f subject) state in
    let%map test' =
      Fuzz.Subject.Test.to_litmus (Fuzz.Output.subject output) ~vars
    in
    (test', Fuzz.Output.metadata output))

let run_with_channels ?(path : string option) (ic : In_channel.t)
    (oc : Out_channel.t) ~(o : Act_common.Output.t)
    ~(f : Fuzz.Subject.Test.t -> 'm Fuzz.Output.t Fuzz.State.Monad.t) :
    'm Or_error.t =
  Or_error.Let_syntax.(
    let%bind test = Litmus_c.Frontend.Fir.load_from_ic ?path ic in
    let%map test', metadata = run_on_litmus ~o ~f test in
    Utils.My_format.fdump oc (Fmt.vbox Litmus_c.Reify.pp_litmus) test' ;
    metadata)

module Randomised = Plumbing.Filter.Make (struct
  type aux_i = Aux.Randomised.t

  type aux_o = Fuzz.Trace.t

  let name = "Fuzzer (random)"

  let run (ctx : Aux.Randomised.t Plumbing.Filter_context.t)
      (ic : In_channel.t) (oc : Out_channel.t) : Fuzz.Trace.t Or_error.t =
    let {Aux.Randomised.seed; o; config} = Plumbing.Filter_context.aux ctx in
    let input = Plumbing.Filter_context.input ctx in
    run_with_channels
      ~path:(Plumbing.Input.to_string input)
      ic oc ~o
      ~f:(Randomised.run ?seed ~config)
end)

let resolve_action (id : Act_common.Id.t) : Fuzz.Action.t Or_error.t =
  Or_error.(
    id
    |> Utils.My_map.find_or_error ~sexp_of_key:Act_common.Id.sexp_of_t
         ~map_name:"action map"
         (Lazy.force Act_fuzz_actions.Table.action_map)
    >>| Fuzz.Action.With_default_weight.action)

let run_replay (subject : Fuzz.Subject.Test.t) ~(trace : Fuzz.Trace.t) :
    unit Fuzz.Output.t Fuzz.State.Monad.t =
  Fuzz.State.Monad.(
    Let_syntax.(
      let%map subject' =
        Fuzz.Trace.run trace subject ~resolve:resolve_action
      in
      Fuzz.Output.make ~subject:subject' ~metadata:()))

module Replay = Plumbing.Filter.Make (struct
  type aux_i = Aux.Replay.t

  type aux_o = unit

  let name = "Fuzzer (replay)"

  let run (ctx : Aux.Replay.t Plumbing.Filter_context.t) (ic : In_channel.t)
      (oc : Out_channel.t) : unit Or_error.t =
    let {Aux.Replay.o; trace} = Plumbing.Filter_context.aux ctx in
    let input = Plumbing.Filter_context.input ctx in
    run_with_channels
      ~path:(Plumbing.Input.to_string input)
      ic oc ~o ~f:(run_replay ~trace)
end)
