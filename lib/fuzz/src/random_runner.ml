(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Ac = Act_common
module State_list = Tx.List.On_monad (State.Monad)

(** [make_rng seed] creates a splittable RNG; if [seed] is [Some s], [s] will
    be used as the RNG's seed, otherwise a low-entropy system-derived seed is
    used. *)
let make_rng : int option -> Splittable_random.State.t = function
  | Some seed ->
      Splittable_random.State.of_int seed
  | None ->
      Splittable_random.State.create (Random.State.make_self_init ())

(** Tracks parts of the fuzzer state and environment that we can't keep in
    the inner state monad for various reasons (circular dependencies, being
    specific to how we choose actions, etc). *)
module Runner_state = struct
  type t =
    {pool: Action.Pool.t; random: Splittable_random.State.t; trace: Trace.t}
  [@@deriving fields]

  module Monad = Travesty.State_transform.Make (struct
    type nonrec t = t

    module Inner = State.Monad
  end)
end

let output () : Ac.Output.t Runner_state.Monad.t =
  Runner_state.Monad.Monadic.return (State.Monad.output ())

let generate_payload (type rs)
    (module Act : Action_types.S with type Payload.t = rs)
    (subject : Subject.Test.t) : rs Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind o = output () in
    let%bind random = Runner_state.Monad.peek Runner_state.random in
    Ac.Output.pv o "fuzz: generating random state for %a...@." Ac.Id.pp
      Act.name ;
    let%map g =
      Runner_state.Monad.Monadic.return (Act.Payload.gen subject ~random)
    in
    Ac.Output.pv o "fuzz: done generating random state.@." ;
    g)

let pick_action (subject : Subject.Test.t) :
    (module Action_types.S) Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind {pool; random; _} = Runner_state.Monad.peek Fn.id in
    Runner_state.Monad.Monadic.return
      (Action.Pool.pick pool ~subject ~random))

let log_action (type p)
    (action : (module Action_types.S with type Payload.t = p)) (payload : p)
    : unit Runner_state.Monad.t =
  Runner_state.Monad.modify (fun x ->
      {x with trace= Trace.add x.trace ~action ~payload})

let run_action (module Action : Action_types.S) (subject : Subject.Test.t) :
    Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    (* We can't, AFAIK, move the payload out of this function, as then the
       subject step runner would become dependent on the exact type of the
       payload, and this is encapsulated in [Action]. *)
    let%bind payload = generate_payload (module Action) subject in
    let%bind () = log_action (module Action) payload in
    Runner_state.Monad.Monadic.return (Action.run subject ~payload))

let mutate_subject_step (subject : Subject.Test.t) :
    Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind o = output () in
    Ac.Output.pv o "fuzz: picking action...@." ;
    let%bind action = pick_action subject in
    Ac.Output.pv o "fuzz: done; now running action...@." ;
    run_action action subject
    >>= Runner_state.Monad.tee ~f:(fun _ ->
            Ac.Output.pv o "fuzz: action done.@."))

let mutate_subject (subject : Subject.Test.t) :
    Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let cap = 10 in
    let%map _, subject' =
      Runner_state.Monad.fix (cap, subject)
        ~f:(fun mu (remaining, subject') ->
          if Int.(remaining = 0) then return (remaining, subject')
          else
            let%bind subject'' = mutate_subject_step subject' in
            mu (remaining - 1, subject''))
    in
    subject')

let make_runner_state (seed : int option) (config : Config.t) :
    Runner_state.t Or_error.t =
  let random = make_rng seed in
  let trace = Trace.empty in
  Or_error.Let_syntax.(
    let%map pool = Config.make_pool config in
    {Runner_state.random; trace; pool})

let make_output (rstate : Runner_state.t) (subject : Subject.Test.t) :
    Trace.t Output.t =
  let trace = Runner_state.trace rstate in
  Output.make ~subject ~metadata:trace

let run ?(seed : int option) (subject : Subject.Test.t) ~(config : Config.t)
    : Trace.t Output.t State.Monad.t =
  State.Monad.(
    Let_syntax.(
      let%bind runner_state =
        Monadic.return (make_runner_state seed config)
      in
      let%map state, subject' =
        Runner_state.Monad.run' (mutate_subject subject) runner_state
      in
      make_output state subject'))
