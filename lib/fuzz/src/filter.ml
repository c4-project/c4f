(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
module Pb = Plumbing

module Aux = struct
  type 'rest t =
    { o: Act_common.Output.t [@default Act_common.Output.silent ()]
    ; config: Config.t
    ; rest: 'rest [@main] }
  [@@deriving make]
end

let run_on_litmus (test : Act_c_mini.Litmus.Test.t)
    ~(o : Act_common.Output.t)
    ~(f : Subject.Test.t -> 'm Output.t State.Monad.t) :
    (Act_c_mini.Litmus.Test.t * 'm) Or_error.t =
  let subject = Subject.Test.of_litmus test in
  Or_error.Let_syntax.(
    let%bind state = State.of_litmus ~o test in
    let%bind state', output = State.Monad.run' (f subject) state in
    let vars = State.vars state' in
    let%map test' = Subject.Test.to_litmus (Output.subject output) ~vars in
    (test', Output.metadata output))

let run_with_channels ?(path : string option) (ic : In_channel.t)
    (oc : Out_channel.t) ~(o : Act_common.Output.t)
    ~(f : Subject.Test.t -> 'm Output.t State.Monad.t) : 'm Or_error.t =
  Or_error.Let_syntax.(
    let%bind test = Act_c_mini.Frontend.load_from_ic ?path ic in
    let%map test', metadata = run_on_litmus ~o ~f test in
    Act_c_mini.Litmus.Pp.print oc test' ;
    metadata)

module Random = Pb.Filter.Make (struct
  type aux_i = int option Aux.t

  type aux_o = Trace.t

  let name = "Fuzzer (random)"

  let run (ctx : int option Aux.t Pb.Filter_context.t) (ic : In_channel.t)
      (oc : Out_channel.t) : Trace.t Or_error.t =
    let {Aux.rest; o; config} = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    run_with_channels
      ~path:(Pb.Input.to_string input)
      ic oc ~o
      ~f:(Random_runner.run ?seed:rest ~config)
end)

(* TODO(@MattWindsor91): unify this logic with all the other resolvers? *)
let resolve_action (id : Act_common.Id.t) :
    (module Action_types.S) Or_error.t =
  Act_utils.My_map.find_or_error ~sexp_of_key:Act_common.Id.sexp_of_t
    ~map_name:"module map"
    (Lazy.force Config.module_map)
    id

let run_replay (subject : Subject.Test.t) ~(trace : Trace.t) :
    unit Output.t State.Monad.t =
  State.Monad.(
    Let_syntax.(
      let%map subject' = Trace.run trace subject ~resolve:resolve_action in
      Output.make ~subject:subject' ~metadata:()))

module Replay = Pb.Filter.Make (struct
  type aux_i = Trace.t Aux.t

  type aux_o = unit

  let name = "Fuzzer (replay)"

  let run (ctx : Trace.t Aux.t Pb.Filter_context.t) (ic : In_channel.t)
      (oc : Out_channel.t) : unit Or_error.t =
    let {Aux.rest; o; _} = Pb.Filter_context.aux ctx in
    let input = Pb.Filter_context.input ctx in
    run_with_channels
      ~path:(Pb.Input.to_string input)
      ic oc ~o ~f:(run_replay ~trace:rest)
end)
