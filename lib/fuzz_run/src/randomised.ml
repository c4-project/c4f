(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

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
    { pool: Fuzz.Action.Pool.t
    ; random: Splittable_random.State.t
    ; trace: Fuzz.Trace.t
    ; params: Fuzz.Param_map.t }
  [@@deriving fields]

  module Monad = Travesty.State_transform.Make (struct
    type nonrec t = t

    module Inner = Fuzz.State.Monad
  end)

  let make_actx (subject : Fuzz.Subject.Test.t) :
      Fuzz.Availability.Context.t Monad.t =
    Monad.(
      peek (fun x -> x.params)
      >>= fun params ->
      Monadic.return
        (Fuzz.State.Monad.peek (fun state ->
             {Fuzz.Availability.Context.state; subject; params})))

  let make_gen_context (action_id : Common.Id.t)
      (subject : Fuzz.Subject.Test.t) : Fuzz.Payload_gen.Context.t Monad.t =
    Monad.(
      Let_syntax.(
        let%bind actx = make_actx subject in
        let%map random = peek (fun x -> x.random) in
        {Fuzz.Payload_gen.Context.action_id; random; actx}))

  let return_err (x : 'a Or_error.t) : 'a Monad.t =
    Monad.Monadic.return (Fuzz.State.Monad.Monadic.return x)
end

let output () : Common.Output.t Runner_state.Monad.t =
  Runner_state.Monad.Monadic.return (Fuzz.State.Monad.output ())

let generate_payload (type rs)
    (module Act : Fuzz.Action_types.S with type Payload.t = rs)
    (subject : Fuzz.Subject.Test.t) : rs Runner_state.Monad.t =
  Runner_state.(
    Monad.(
      Let_syntax.(
        let%bind o = output () in
        let%bind ctx = make_gen_context Act.name subject in
        Common.Output.pv o "fuzz: generating random state for %a...@."
          Common.Id.pp Act.name ;
        let%map g = return_err (Fuzz.Payload_gen.run Act.Payload.gen ~ctx) in
        Common.Output.pv o "fuzz: done generating random state.@." ;
        g)))

let available (module A : Fuzz.Action_types.S)
    ~(subject : Fuzz.Subject.Test.t) : bool Runner_state.Monad.t =
  Runner_state.(
    Monad.(
      make_actx subject
      >>= fun ctx -> return_err (Fuzz.Availability.M.run A.available ~ctx)))

let pick (pool : Fuzz.Action.Pool.t) :
    (Fuzz.Action.Pool.t * (module Fuzz.Action_types.S)) Runner_state.Monad.t
    =
  Runner_state.Monad.(
    Let_syntax.(
      let%bind random = peek Runner_state.random in
      Runner_state.return_err (Fuzz.Action.Pool.pick pool ~random)))

let rec pick_loop (pool : Fuzz.Action.Pool.t)
    ~(subject : Fuzz.Subject.Test.t) :
    (module Fuzz.Action_types.S) Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind pool', action = pick pool in
    if%bind available action ~subject then return action
    else pick_loop pool' ~subject)

let pick_action (subject : Fuzz.Subject.Test.t) :
    (module Fuzz.Action_types.S) Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind pool = Runner_state.(Monad.peek pool) in
    pick_loop pool ~subject)

let log_action (type p)
    (action : (module Fuzz.Action_types.S with type Payload.t = p))
    (payload : p) : unit Runner_state.Monad.t =
  Runner_state.Monad.modify (fun x ->
      {x with trace= Fuzz.Trace.add x.trace ~action ~payload})

let run_action (module Action : Fuzz.Action_types.S)
    (subject : Fuzz.Subject.Test.t) :
    Fuzz.Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    (* We can't, AFAIK, move the payload out of this function, as then the
       subject step runner would become dependent on the exact type of the
       payload, and this is encapsulated in [Action]. *)
    let%bind payload = generate_payload (module Action) subject in
    let%bind () = log_action (module Action) payload in
    Runner_state.Monad.Monadic.return (Action.run subject ~payload))

let mutate_subject_step (subject : Fuzz.Subject.Test.t) :
    Fuzz.Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind o = output () in
    Common.Output.pv o "fuzz: picking action...@." ;
    let%bind action = pick_action subject in
    Common.Output.pv o "fuzz: done; now running action...@." ;
    run_action action subject
    >>= Runner_state.Monad.tee ~f:(fun _ ->
            Common.Output.pv o "fuzz: action done.@."))

let get_cap () : int Runner_state.Monad.t =
  Runner_state.Monad.(
    Let_syntax.(
      let%bind param_map = peek (fun x -> x.params) in
      Monadic.return
        Fuzz.State.Monad.(
          Monadic.return (Fuzz.Param_map.get_action_cap param_map))))

let mutate_subject (subject : Fuzz.Subject.Test.t) :
    Fuzz.Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind cap = get_cap () in
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
  let trace = Fuzz.Trace.empty in
  Or_error.Let_syntax.(
    let%map pool = Config.make_pool config in
    let params = Config.make_param_map config in
    {Runner_state.random; trace; pool; params})

let make_output (rstate : Runner_state.t) (subject : Fuzz.Subject.Test.t) :
    Fuzz.Trace.t Fuzz.Output.t =
  let trace = Runner_state.trace rstate in
  Fuzz.Output.make ~subject ~metadata:trace

let run ?(seed : int option) (subject : Fuzz.Subject.Test.t)
    ~(config : Config.t) : Fuzz.Trace.t Fuzz.Output.t Fuzz.State.Monad.t =
  Fuzz.State.Monad.(
    Let_syntax.(
      let%bind runner_state =
        Monadic.return (make_runner_state seed config)
      in
      let%map state, subject' =
        Runner_state.Monad.run' (mutate_subject subject) runner_state
      in
      make_output state subject'))
