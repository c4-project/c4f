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

(** [make_rng seed] creates a splittable RNG; if [seed] is [Some s], [s]
    will be used as the RNG's seed, otherwise a low-entropy system-derived
    seed is used. *)
let make_rng : int option -> Splittable_random.State.t = function
  | Some seed ->
      Splittable_random.State.of_int seed
  | None ->
      Splittable_random.State.create (Random.State.make_self_init ())

let modules : (module Action_types.S) list Lazy.t =
  lazy
    [ (module Var_actions.Make_global : Action_types.S)
    ; (module Store_actions.Int : Action_types.S)
    ; (module Program_actions.Make_empty : Action_types.S) ]

(** Tracks parts of the fuzzer state and environment that we can't keep in
    the inner state monad for various reasons (circular dependencies, being
    specific to how we choose actions, etc). *)
module Outer_state = struct
  type t =
    {pool: Action.Pool.t; random: Splittable_random.State.t; trace: Trace.t}
  [@@deriving fields]

  module Monad = Travesty.State_transform.Make (struct
    type nonrec t = t

    module Inner = State.Monad
  end)
end

let output () : Ac.Output.t Outer_state.Monad.t =
  Outer_state.Monad.Monadic.return (State.Monad.output ())

let generate_payload (type rs)
    (module Act : Action_types.S with type Random_state.t = rs)
    (subject : Subject.Test.t) : rs Outer_state.Monad.t =
  let open Outer_state.Monad.Let_syntax in
  let%bind o = output () in
  let%bind random = Outer_state.Monad.peek Outer_state.random in
  Ac.Output.pv o "fuzz: generating random state for %a...@." Ac.Id.pp
    Act.name ;
  let%map g =
    Outer_state.Monad.Monadic.return (Act.Random_state.gen subject ~random)
  in
  Ac.Output.pv o "fuzz: done generating random state.@." ;
  g

let pick_action (subject : Subject.Test.t) :
    (module Action_types.S) Outer_state.Monad.t =
  Outer_state.Monad.Let_syntax.(
    let%bind {pool; random; _} = Outer_state.Monad.peek Fn.id in
    Outer_state.Monad.Monadic.return (Action.Pool.pick pool subject random))

let log_action (type p)
    (action : (module Action_types.S with type Random_state.t = p))
    (payload : p) : unit Outer_state.Monad.t =
  Outer_state.Monad.modify (fun x ->
      {x with trace= Trace.add x.trace ~action ~payload})

let run_action (subject : Subject.Test.t) (module Action : Action_types.S) :
    Subject.Test.t Outer_state.Monad.t =
  Outer_state.Monad.Let_syntax.(
    (* We can't, AFAIK, move the payload out of this function, as then the
       subject step runner would become dependent on the exact type of the
       payload, and this is encapsulated in [Action]. *)
    let%bind payload = generate_payload (module Action) subject in
    let%bind () = log_action (module Action) payload in
    Outer_state.Monad.Monadic.return (Action.run subject payload))

let mutate_subject_step (subject : Subject.Test.t) :
    Subject.Test.t Outer_state.Monad.t =
  Outer_state.Monad.Let_syntax.(
    let%bind o = output () in
    Ac.Output.pv o "fuzz: picking action...@." ;
    let%bind action = pick_action subject in
    Ac.Output.pv o "fuzz: done; now running action...@." ;
    run_action subject action
    >>= Outer_state.Monad.tee ~f:(fun _ ->
            Ac.Output.pv o "fuzz: action done.@."))

let make_pool : Act_config.Fuzz.t -> Action.Pool.t Or_error.t =
  Action.Pool.make (Lazy.force modules)

let summarise (cfg : Act_config.Fuzz.t) :
    Action.Summary.t Ac.Id.Map.t Or_error.t =
  Or_error.(cfg |> make_pool >>| Action.Pool.summarise)

let mutate_subject (subject : Subject.Test.t) :
    Subject.Test.t Outer_state.Monad.t =
  Outer_state.Monad.Let_syntax.(
    let cap = 10 in
    let%map _, subject' =
      Outer_state.Monad.fix (cap, subject)
        ~f:(fun mu (remaining, subject') ->
          if Int.(remaining = 0) then return (remaining, subject')
          else
            let%bind subject'' = mutate_subject_step subject' in
            mu (remaining - 1, subject''))
    in
    subject')

let run_with_state (test : Act_c_mini.Litmus.Ast.Validated.t) :
    Act_c_mini.Litmus.Ast.Validated.t Outer_state.Monad.t =
  Outer_state.Monad.Let_syntax.(
    (* TODO: add uuid to this *)
    let name = Act_c_mini.Litmus.Ast.Validated.name test in
    let postcondition =
      Act_c_mini.Litmus.Ast.Validated.postcondition test
    in
    let subject = Subject.Test.of_litmus test in
    let%bind subject' = mutate_subject subject in
    Outer_state.Monad.Monadic.return
      State.Monad.(
        with_vars_m (fun vars ->
            Monadic.return
              (Subject.Test.to_litmus ~vars ~name ?postcondition subject'))))

(** [get_first_func test] tries to get the first function in a validated
    litmus AST [test]. *)
let get_first_func (test : Act_c_mini.Litmus.Ast.Validated.t) :
    Act_c_mini.Function.t Or_error.t =
  Or_error.(
    (* If this is a validated litmus test, it _should_ have at least one
       function, and each function _should_ report the right global
       variables. *)
    test |> Act_c_mini.Litmus.Ast.Validated.programs |> List.hd
    |> Result.of_option
         ~error:
           (Error.of_string
              "Internal error: validated litmus had no functions")
    >>| Act_c_mini.Named.value)

(** [existing_globals test] extracts the existing global variable names and
    types from litmus test [test]. *)
let existing_globals (test : Act_c_mini.Litmus.Ast.Validated.t) :
    Act_c_mini.Type.t Map.M(Ac.C_id).t Or_error.t =
  Or_error.(
    test |> get_first_func >>| Act_c_mini.Function.parameters
    >>= Map.of_alist_or_error (module Ac.C_id))

let to_locals : Set.M(Ac.Litmus_id).t -> Set.M(Ac.Litmus_id).t =
  (* TODO(@MattWindsor91): do we need to keep the thread IDs? *)
  Set.filter ~f:Ac.Litmus_id.is_local

let make_inner_state (o : Ac.Output.t)
    (test : Act_c_mini.Litmus.Ast.Validated.t) : State.t Or_error.t =
  Or_error.Let_syntax.(
    let all_vars = Act_c_mini.Litmus.vars test in
    let%map globals = existing_globals test in
    let locals = to_locals all_vars in
    State.init ~o ~globals ~locals ())

let make_outer_state (seed : int option) (config : Act_config.Fuzz.t) :
    Outer_state.t Or_error.t =
  let random = make_rng seed in
  let trace = Trace.empty in
  Or_error.Let_syntax.(
    let%map pool = make_pool config in
    {Outer_state.random; trace; pool})

let run ?(seed : int option) (test : Act_c_mini.Litmus.Ast.Validated.t)
    ~(o : Ac.Output.t) ~(config : Act_config.Fuzz.t) :
    (Act_c_mini.Litmus.Ast.Validated.t * Trace.t) Or_error.t =
  Or_error.Let_syntax.(
    let%bind inner_state = make_inner_state o test in
    let%bind outer_state = make_outer_state seed config in
    let%map outer_state', test' =
      State.Monad.run
        (Outer_state.Monad.run' (run_with_state test) outer_state)
        inner_state
    in
    (test', outer_state'.trace))
