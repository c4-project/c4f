(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck

type 'a stateful_gen =
     Subject.Test.t
  -> random:Splittable_random.State.t
  -> param_map:Param_map.t
  -> 'a State.Monad.t

module Helpers = struct
  let lift_quickcheck (type a) (gen : a Q.Generator.t)
      ~(random : Splittable_random.State.t) : a State.Monad.t =
    State.Monad.return (Q.Generator.generate ~size:10 ~random gen)

  let lift_quickcheck_opt (type a) (gen_opt : a Opt_gen.t)
      ~(random : Splittable_random.State.t) ~(action_id : Act_common.Id.t) :
      a State.Monad.t =
    State.Monad.Let_syntax.(
      let%bind gen =
        State.Monad.Monadic.return
          (Or_error.tag_s gen_opt
             ~tag:
               [%message
                 "Payload generator instantiation failed."
                   ~action_id:(action_id : Act_common.Id.t)])
      in
      lift_quickcheck gen ~random)

  let lift_path_gen (test : Subject.Test.t)
      ~(random : Splittable_random.State.t) ~(action_id : Act_common.Id.t)
      ~(filter : Path_filter.t State.Monad.t)
      ~(f :
         ?filter:Path_filter.t -> Subject.Test.t -> Path.Program.t Opt_gen.t)
      : Path.Program.t State.Monad.t =
    State.Monad.Let_syntax.(
      let%bind filter = filter in
      lift_quickcheck_opt (f test ~filter) ~random ~action_id)
end

module Insertion = struct
  type 'a t = {to_insert: 'a; where: Path.Program.t}
  [@@deriving make, fields, sexp]

  module Make (Basic : sig
    type t [@@deriving sexp]

    val name : Act_common.Id.t

    val path_filter : Path_filter.t State.Monad.t

    val gen : Path.Program.t -> t stateful_gen
  end) =
  struct
    open struct
      type 'a ins = 'a t [@@deriving sexp]
    end

    type t = Basic.t ins [@@deriving sexp]

    let gen_path test =
      Helpers.lift_path_gen test ~filter:Basic.path_filter
        ~action_id:Basic.name ~f:Path_producers.try_gen_insert_stm

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : t State.Monad.t =
      (* We can't do this inside the Opt-gen applicative, as we need the
         ability to pull out the path and use it in the main generator. *)
      State.Monad.Let_syntax.(
        let%bind where = gen_path test ~random in
        let%map to_insert = Basic.gen where test ~random ~param_map in
        make ~to_insert ~where)
  end
end

module Cond_surround = struct
  module Body = struct
    type t = {cond: Act_fir.Expression.t; where: Path.Program.t}
    [@@deriving make, sexp, fields]
  end

  include Body

  (** [add_cond_dependencies path cond] marks every variable in [cond] as
      having a read dependency, using [path] to resolve the scope at which
      [cond] is being inserted.

      This is a considerable over-approximation of the actual needed
      dependencies. *)
  let add_cond_dependencies (path : Path.Program.t)
      (cond : Act_fir.Expression.t) : unit State.Monad.t =
    (* TODO(@MattWindsor91): it would be pretty cool for the lvalues to track
       whether or not they are being used in a tautology, once we add
       metadata tracking to the expression. In this case, we could skip
       adding dependencies where not necessary. *)
    State.Monad.add_expression_dependencies cond
      ~scope:(Local (Path.Program.tid path))

  let apply ({cond; where} : t) ~(test : Subject.Test.t)
      ~(f :
            Act_fir.Expression.t
         -> Subject.Statement.t list
         -> Subject.Statement.t) : Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () = add_cond_dependencies where cond in
        Monadic.return
          (Path_consumers.Test.transform_stm_list where ~target:test
             ~f:(fun test -> Ok [f cond test]))))

  let cond_env (vars : Var.Map.t) ~(tid : int) : Act_fir.Env.t =
    Var.Map.env_satisfying_all ~scope:(Local tid) ~predicates:[] vars

  module Make (Basic : sig
    val name : Act_common.Id.t
    (** [name] should be the name of the action. *)

    val cond_gen : Act_fir.Env.t -> Act_fir.Expression.t Q.Generator.t
    (** [cond_gen env] should, given a first-class environment module [env]
        capturing the variables in scope at the point where the if statement
        is appearing, return a Quickcheck generator generating expressions
        over those variables. *)

    val path_filter : Path_filter.t State.Monad.t
  end) : Action_types.S_payload with type t = Body.t = struct
    include Body

    let gen_path test =
      Helpers.lift_path_gen test ~filter:Basic.path_filter
        ~action_id:Basic.name ~f:Path_producers.try_gen_transform_stm_list

    let quickcheck_cond (path : Path.Program.t) :
        Act_fir.Expression.t Q.Generator.t State.Monad.t =
      let tid = Path.Program.tid path in
      State.Monad.Let_syntax.(
        let%map env = State.Monad.with_vars (cond_env ~tid) in
        Basic.cond_gen env)

    let gen_cond (path : Path.Program.t)
        ~(random : Splittable_random.State.t) :
        Act_fir.Expression.t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind gen = quickcheck_cond path in
        Helpers.lift_quickcheck gen ~random)

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : Body.t State.Monad.t =
      ignore param_map ;
      State.Monad.Let_syntax.(
        let%bind where = gen_path test ~random in
        let%map cond = gen_cond where ~random in
        Body.make ~cond ~where)
  end
end

module None : Action_types.S_payload with type t = unit = struct
  include Unit

  let gen (_ : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) =
    ignore (random : Splittable_random.State.t) ;
    ignore (param_map : Param_map.t) ;
    State.Monad.return ()
end

module Pure (S : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Q.Generator.t
end) : Action_types.S_payload with type t = S.t = struct
  include S

  let gen (_subject : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) : t State.Monad.t =
    ignore (param_map : Param_map.t) ;
    Helpers.lift_quickcheck S.quickcheck_generator ~random
end
