(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

open struct
  module Pb = Plumbing
  module Fir = Act_fir
  module F = Act_fuzz
end

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
      ; trace: F.Trace.t [@main] }
    [@@deriving make]
  end
end

let run_on_litmus (test : Fir.Litmus.Test.t) ~(o : Act_common.Output.t)
    ~(f : F.Subject.Test.t -> 'm F.Output.t F.State.Monad.t) :
    (Fir.Litmus.Test.t * 'm) Or_error.t =
  let subject = F.Subject.Test.of_litmus test in
  Or_error.Let_syntax.(
    let%bind state = F.State.of_litmus ~o test in
    let%bind state', output = F.State.Monad.run' (f subject) state in
    let vars = F.State.vars state' in
    let%map test' =
      F.Subject.Test.to_litmus (F.Output.subject output) ~vars
    in
    (test', F.Output.metadata output))

let run_with_channels ?(path : string option) (ic : In_channel.t)
    (oc : Out_channel.t) ~(o : Act_common.Output.t)
    ~(f : F.Subject.Test.t -> 'm F.Output.t F.State.Monad.t) : 'm Or_error.t
    =
  Or_error.Let_syntax.(
    let%bind test = Act_litmus_c.Frontend.Fir.load_from_ic ?path ic in
    let%map test', metadata = run_on_litmus ~o ~f test in
    Act_utils.My_format.fdump oc
      (Fmt.vbox Act_litmus_c.Reify.pp_litmus)
      test' ;
    metadata)

module Randomised = Pb.Filter.Make (struct
  type aux_i = Aux.Randomised.t

  type aux_o = F.Trace.t

  let name = "Fuzzer (random)"

  let run (ctx : Aux.Randomised.t Pb.Filter_context.t) (ic : In_channel.t)
      (oc : Out_channel.t) : F.Trace.t Or_error.t =
    let {Aux.Randomised.seed; o; config} = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    run_with_channels
      ~path:(Pb.Input.to_string input)
      ic oc ~o
      ~f:(Randomised.run ?seed ~config)
end)

(* TODO(@MattWindsor91): unify this logic with all the other resolvers? *)
let resolve_action (id : Act_common.Id.t) : F.Action.t Or_error.t =
  Or_error.(
    id
    |> Act_utils.My_map.find_or_error ~sexp_of_key:Act_common.Id.sexp_of_t
         ~map_name:"action map"
         (Lazy.force Act_fuzz_actions.Table.action_map)
    >>| F.Action.With_default_weight.action)

let run_replay (subject : F.Subject.Test.t) ~(trace : F.Trace.t) :
    unit F.Output.t F.State.Monad.t =
  F.State.Monad.(
    Let_syntax.(
      let%map subject' = F.Trace.run trace subject ~resolve:resolve_action in
      F.Output.make ~subject:subject' ~metadata:()))

module Replay = Pb.Filter.Make (struct
  type aux_i = Aux.Replay.t

  type aux_o = unit

  let name = "Fuzzer (replay)"

  let run (ctx : Aux.Replay.t Pb.Filter_context.t) (ic : In_channel.t)
      (oc : Out_channel.t) : unit Or_error.t =
    let {Aux.Replay.o; trace} = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    run_with_channels
      ~path:(Pb.Input.to_string input)
      ic oc ~o ~f:(run_replay ~trace)
end)
