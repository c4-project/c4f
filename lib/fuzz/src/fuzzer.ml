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

(** Fuzzer action that generates a new global variable. *)
module Make_global : Action_types.S = struct
  let name = Ac.Id.of_string "var.make.global"

  let readme () =
    Act_utils.My_string.format_for_readme
      {|
    Generates a new global integer variable, with a random name, initial value,
    and atomicity.
    |}

  let default_weight = 2

  module Random_state = struct
    type t = {is_atomic: bool; initial_value: int; name: Ac.C_id.t}
    [@@deriving sexp]

    module G = Base_quickcheck.Generator

    let gen' (vars : Var.Map.t) : t G.t =
      let open G.Let_syntax in
      let%bind is_atomic = G.bool in
      let%bind initial_value =
        Act_c_lang.Ast_basic.Constant.gen_int32_as_int
      in
      let%map name = Var.Map.gen_fresh_var vars in
      {is_atomic; initial_value; name}

    let gen (_subject : Subject.Test.t) : t G.t State.Monad.t =
      State.Monad.with_vars gen'
  end

  let available = Action.always

  let run (subject : Subject.Test.t)
      ({is_atomic; initial_value; name} : Random_state.t) :
      Subject.Test.t State.Monad.t =
    let ty = Act_c_mini.Type.int_type ~is_atomic ~is_pointer:true in
    let open State.Monad.Let_syntax in
    let%map () =
      State.Monad.register_global ty name
        ~initial_value:(Var.Value.Int initial_value)
    in
    let const = Act_c_lang.Ast_basic.Constant.Integer initial_value in
    Subject.Test.add_var_to_init subject name const
end

let generate_random_state (type rs)
    (module Act : Action_types.S with type Random_state.t = rs)
    (subject : Subject.Test.t) (random : Splittable_random.State.t) :
    rs State.Monad.t =
  let open State.Monad.Let_syntax in
  let%bind o = State.Monad.output () in
  Ac.Output.pv o "fuzz: getting random state generator for %a@." Ac.Id.pp
    Act.name ;
  let%map gen = Act.Random_state.gen subject in
  Ac.Output.pv o "fuzz: generating random state for %a...@." Ac.Id.pp
    Act.name ;
  let g = Base_quickcheck.Generator.generate gen ~random ~size:10 in
  Ac.Output.pv o "fuzz: done generating random state.@." ;
  g

let run_action (module Act : Action_types.S) (subject : Subject.Test.t)
    (rng : Splittable_random.State.t) : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let%bind state = generate_random_state (module Act) subject rng in
  Act.run subject state

let modules : (module Action_types.S) list Lazy.t =
  lazy
    [ (module Make_global : Action_types.S)
    ; (module Store_actions.Int : Action_types.S)
    ; (module Program_actions.Make_empty : Action_types.S) ]

let mutate_subject_step (pool : Action.Pool.t) (subject : Subject.Test.t)
    (rng : Splittable_random.State.t) : Subject.Test.t State.Monad.t =
  let open State.Monad.Let_syntax in
  let%bind o = State.Monad.output () in
  Ac.Output.pv o "fuzz: picking action...@." ;
  let%bind action = Action.Pool.pick pool subject rng in
  Ac.Output.pv o "fuzz: done; now running action...@." ;
  run_action action subject rng
  >>= State.Monad.tee_m ~f:(fun _ ->
          Ac.Output.pv o "fuzz: action done.@." ;
          State.Monad.return ())

let make_pool : Act_config.Fuzz.t -> Action.Pool.t Or_error.t =
  Action.Pool.make (Lazy.force modules)

let summarise (cfg : Act_config.Fuzz.t) :
    Action.Summary.t Ac.Id.Map.t Or_error.t =
  Or_error.(cfg |> make_pool >>| Action.Pool.summarise)

let mutate_subject (subject : Subject.Test.t) ~(config : Act_config.Fuzz.t)
    ~(rng : Splittable_random.State.t) : Subject.Test.t State.Monad.t =
  State.Monad.Let_syntax.(
    let cap = 10 in
    let%bind pool = State.Monad.Monadic.return (make_pool config) in
    let%map _, subject' =
      State.Monad.fix (cap, subject) ~f:(fun mu (remaining, subject') ->
          if Int.(remaining = 0) then return (remaining, subject')
          else
            let%bind subject'' = mutate_subject_step pool subject' rng in
            mu (remaining - 1, subject''))
    in
    subject')

let run_with_state (test : Act_c_mini.Litmus.Ast.Validated.t)
    ~(config : Act_config.Fuzz.t) ~(rng : Splittable_random.State.t) :
    Act_c_mini.Litmus.Ast.Validated.t State.Monad.t =
  let open State.Monad.Let_syntax in
  (* TODO: add uuid to this *)
  let name = Act_c_mini.Litmus.Ast.Validated.name test in
  let postcondition = Act_c_mini.Litmus.Ast.Validated.postcondition test in
  let subject = Subject.Test.of_litmus test in
  let%bind subject' = mutate_subject subject ~config ~rng in
  State.Monad.with_vars_m (fun vars ->
      State.Monad.Monadic.return
        (Subject.Test.to_litmus ~vars ~name ?postcondition subject'))

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
    Act_c_mini.Type.t Ac.C_id.Map.t Or_error.t =
  Or_error.(
    test |> get_first_func >>| Act_c_mini.Function.parameters
    >>= Ac.C_id.Map.of_alist_or_error)

let to_locals : Set.M(Ac.Litmus_id).t -> Set.M(Ac.C_id).t =
  (* TODO(@MattWindsor91): do we need to keep the thread IDs? *)
  Set.filter_map
    (module Ac.C_id)
    ~f:(Fn.compose (Option.map ~f:snd) Ac.Litmus_id.as_local)

let make_initial_state (o : Ac.Output.t)
    (test : Act_c_mini.Litmus.Ast.Validated.t) : State.t Or_error.t =
  let open Or_error.Let_syntax in
  let all_vars = Act_c_mini.Litmus.vars test in
  (* TODO(@MattWindsor91): we don't use cvars's globals because we need to
     know the types of the variables. This seems a _bit_ clunky. *)
  let%map globals = existing_globals test in
  let locals = to_locals all_vars in
  State.init ~o ~globals ~locals ()

let run ?(seed : int option) (test : Act_c_mini.Litmus.Ast.Validated.t)
    ~(o : Ac.Output.t) ~(config : Act_config.Fuzz.t) :
    Act_c_mini.Litmus.Ast.Validated.t Or_error.t =
  Or_error.(
    make_initial_state o test
    >>= State.Monad.run (run_with_state test ~config ~rng:(make_rng seed)))
