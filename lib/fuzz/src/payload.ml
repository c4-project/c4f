(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck

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
end

module Program_path (Basic : sig
  val action_id : Act_common.Id.t

  val build_filter : Path_filter.t -> Path_filter.t

  val gen :
    ?filter:Path_filter.t -> Subject.Test.t -> Path.Program.t Opt_gen.t
end) : Action_types.S_payload with type t = Path.Program.t = struct
  type t = Path.Program.t [@@deriving sexp]

  let quickcheck_path (test : Subject.Test.t) : Path.Program.t Opt_gen.t =
    let filter = Basic.build_filter Path_filter.empty in
    Basic.gen ~filter test

  let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
      ~(param_map : Param_map.t) : Path.Program.t State.Monad.t =
    ignore (param_map : Param_map.t) ;
    Helpers.lift_quickcheck_opt (quickcheck_path test) ~random
      ~action_id:Basic.action_id
end

module Surround = struct
  module Body = struct
    type t = {cond: Act_c_mini.Expression.t; path: Path.Program.t}
    [@@deriving make, sexp, fields]
  end

  include Body

  (** [add_cond_dependencies path cond] marks every variable in [cond] as
      having a read dependency, using [path] to resolve the scope at which
      [cond] is being inserted.

      This is a considerable over-approximation of the actual needed
      dependencies. *)
  let add_cond_dependencies (path : Path.Program.t)
      (cond : Act_c_mini.Expression.t) : unit State.Monad.t =
    (* TODO(@MattWindsor91): it would be pretty cool for the lvalues to track
       whether or not they are being used in a tautology, once we add
       metadata tracking to the expression. In this case, we could skip
       adding dependencies where not necessary. *)
    State.Monad.add_expression_dependencies cond
      ~scope:(Local (Path.Program.tid path))

  let apply ({cond; path} : t) ~(test : Subject.Test.t)
      ~(f :
            Act_c_mini.Expression.t
         -> Subject.Statement.t list
         -> Subject.Statement.t) : Subject.Test.t State.Monad.t =
    State.Monad.(
      Let_syntax.(
        let%bind () = add_cond_dependencies path cond in
        Monadic.return
          (Path_consumers.Test.transform_stm_list path ~target:test
             ~f:(fun test -> Or_error.return [f cond test]))))

  let cond_env (vars : Var.Map.t) ~(tid : int) :
      (module Act_c_mini.Env_types.S_with_known_values) =
    Var.Map.env_module_with_known_values ~scope:(Local tid) ~predicates:[]
      vars

  module Make (Basic : sig
    val action_id : Act_common.Id.t

    val cond_gen :
         (module Act_c_mini.Env_types.S_with_known_values)
      -> Act_c_mini.Expression.t Q.Generator.t
    (** [cond_gen env] should, given a first-class environment module [env]
        capturing the variables in scope at the point where the if statement
        is appearing, return a Quickcheck generator generating expressions
        over those variables. *)

    val build_filter : Path_filter.t -> Path_filter.t
  end) : Action_types.S_payload with type t = Body.t = struct
    include Body

    module PP = Program_path (struct
      let action_id = Basic.action_id

      let gen = Path_producers.Test.try_gen_transform_stm_list

      let build_filter = Basic.build_filter
    end)

    let quickcheck_cond (path : Path.Program.t) :
        Act_c_mini.Expression.t Q.Generator.t State.Monad.t =
      let tid = Path.Program.tid path in
      State.Monad.Let_syntax.(
        let%map env = State.Monad.with_vars (cond_env ~tid) in
        Basic.cond_gen env)

    let gen_cond (path : Path.Program.t)
        ~(random : Splittable_random.State.t) :
        Act_c_mini.Expression.t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind gen = quickcheck_cond path in
        Helpers.lift_quickcheck gen ~random)

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : Body.t State.Monad.t =
      State.Monad.Let_syntax.(
        let%bind path = PP.gen test ~random ~param_map in
        let%map cond = gen_cond path ~random in
        Body.make ~cond ~path)
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
