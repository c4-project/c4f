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
  module F = Act_fuzz
end

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
    { pool: F.Action.Pool.t
    ; random: Splittable_random.State.t
    ; trace: F.Trace.t
    ; param_map: F.Param_map.t }
  [@@deriving fields]

  module Monad = Travesty.State_transform.Make (struct
    type nonrec t = t

    module Inner = F.State.Monad
  end)

  let make_gen_context (action_id : Ac.Id.t) (subject : F.Subject.Test.t)
      (st : t) : F.Payload_gen.Context.t F.State.Monad.t =
    let param_map = param_map st in
    let random = random st in
    F.State.Monad.peek (fun state ->
        F.Payload_gen.Context.make ~action_id ~subject ~param_map ~state
          ~random)
end

let output () : Ac.Output.t Runner_state.Monad.t =
  Runner_state.Monad.Monadic.return (F.State.Monad.output ())

let generate_payload (type rs)
    (module Act : F.Action_types.S with type Payload.t = rs)
    (subject : F.Subject.Test.t) : rs Runner_state.Monad.t =
  Runner_state.(
    Monad.(
      Let_syntax.(
        let%bind o = output () in
        let%bind ctx = Monadic.peek (make_gen_context Act.name subject) in
        Ac.Output.pv o "fuzz: generating random state for %a...@." Ac.Id.pp
          Act.name ;
        let%map g =
          Monadic.return
            (F.State.Monad.Monadic.return
               (F.Payload_gen.run Act.Payload.gen ~ctx))
        in
        Ac.Output.pv o "fuzz: done generating random state.@." ;
        g)))

let pick_action (subject : F.Subject.Test.t) :
    (module F.Action_types.S) Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind {pool; random; param_map; _} = Runner_state.Monad.peek Fn.id in
    Runner_state.Monad.Monadic.return
      (F.Action.Pool.pick pool ~subject ~random ~param_map))

let log_action (type p)
    (action : (module F.Action_types.S with type Payload.t = p))
    (payload : p) : unit Runner_state.Monad.t =
  Runner_state.Monad.modify (fun x ->
      {x with trace= F.Trace.add x.trace ~action ~payload})

let run_action (module Action : F.Action_types.S)
    (subject : F.Subject.Test.t) : F.Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    (* We can't, AFAIK, move the payload out of this function, as then the
       subject step runner would become dependent on the exact type of the
       payload, and this is encapsulated in [Action]. *)
    let%bind payload = generate_payload (module Action) subject in
    let%bind () = log_action (module Action) payload in
    Runner_state.Monad.Monadic.return (Action.run subject ~payload))

let mutate_subject_step (subject : F.Subject.Test.t) :
    F.Subject.Test.t Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind o = output () in
    Ac.Output.pv o "fuzz: picking action...@." ;
    let%bind action = pick_action subject in
    Ac.Output.pv o "fuzz: done; now running action...@." ;
    run_action action subject
    >>= Runner_state.Monad.tee ~f:(fun _ ->
            Ac.Output.pv o "fuzz: action done.@."))

let get_cap () : int Runner_state.Monad.t =
  Runner_state.Monad.Let_syntax.(
    let%bind param_map = Runner_state.Monad.peek Runner_state.param_map in
    Runner_state.Monad.Monadic.return
      (F.State.Monad.Monadic.return (F.Param_map.get_action_cap param_map)))

let mutate_subject (subject : F.Subject.Test.t) :
    F.Subject.Test.t Runner_state.Monad.t =
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
  let trace = F.Trace.empty in
  let param_map = Config.make_param_map config in
  Or_error.Let_syntax.(
    let%map pool = Config.make_pool config in
    {Runner_state.random; param_map; trace; pool})

let make_output (rstate : Runner_state.t) (subject : F.Subject.Test.t) :
    F.Trace.t F.Output.t =
  let trace = Runner_state.trace rstate in
  F.Output.make ~subject ~metadata:trace

let run ?(seed : int option) (subject : F.Subject.Test.t)
    ~(config : Config.t) : F.Trace.t F.Output.t F.State.Monad.t =
  F.State.Monad.(
    Let_syntax.(
      let%bind runner_state =
        Monadic.return (make_runner_state seed config)
      in
      let%map state, subject' =
        Runner_state.Monad.run' (mutate_subject subject) runner_state
      in
      make_output state subject'))
