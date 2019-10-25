(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module If = struct
  module Payload = struct
    type t = {cond: Act_c_mini.Expression.t; path: Path_shapes.program}
    [@@deriving make, sexp, fields]
  end

  module type S = Action_types.S with type Payload.t = Payload.t

  module Make (Basic : sig
    val name_suffix : string

    val readme_suffix : string

    val t_branch_of_statements : Subject.Statement.t list -> Subject.Block.t

    val f_branch_of_statements : Subject.Statement.t list -> Subject.Block.t

    val cond_gen :
         Path_shapes.program
      -> Act_c_mini.Expression.t Base_quickcheck.Generator.t State.Monad.t
  end) : S = struct
    include Basic

    let name = Act_common.Id.of_string_list ["flow"; "if"; name_suffix]

    let readme () =
      {| Removes a sublist of statements from the program, replacing them
        with an `if` statement containing some transformation of the
        removed statements. |}
      ^ "\n\n" ^ readme_suffix

    module Payload = struct
      include Payload

      (* TODO(@MattWindsor91): the dependency flow from paths to conditions
         is a little hackneyed. It could do with being cleaned up a little,
         maybe. *)

      let quickcheck_path (test : Subject.Test.t) :
          Path_shapes.program Base_quickcheck.Generator.t =
        Option.value_exn (Path.Subject.Test.try_gen_transform_stm_list test)

      let gen_path (test : Subject.Test.t)
          ~(random : Splittable_random.State.t) :
          Path_shapes.program State.Monad.t =
        Action.lift_quickcheck (quickcheck_path test) ~random

      let gen_cond (path : Path_shapes.program)
          ~(random : Splittable_random.State.t) :
          Act_c_mini.Expression.t State.Monad.t =
        State.Monad.Let_syntax.(
          let%bind gen = Basic.cond_gen path in
          Action.lift_quickcheck gen ~random)

      let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
          : Payload.t State.Monad.t =
        State.Monad.Let_syntax.(
          let%bind path = gen_path test ~random in
          let%map cond = gen_cond path ~random in
          Payload.make ~cond ~path)
    end

    let available (test : Subject.Test.t) : bool State.Monad.t =
      test |> Act_litmus.Test.Raw.threads
      |> List.exists ~f:Subject.Program.has_statements
      |> State.Monad.return

    let wrap_in_if_raw (statements : Metadata.t Act_c_mini.Statement.t list)
        ~(cond : Act_c_mini.Expression.t) :
        Metadata.t Act_c_mini.Statement.t =
      Act_c_mini.Statement.if_stm
        (Act_c_mini.Statement.If.make ~cond
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements))

    let wrap_in_if (statements : Metadata.t Act_c_mini.Statement.t list)
        ~(cond : Act_c_mini.Expression.t) :
        Metadata.t Act_c_mini.Statement.t list Or_error.t =
      Or_error.return [wrap_in_if_raw statements ~cond]

    let run (test : Subject.Test.t) ~(payload : Payload.t) :
        Subject.Test.t State.Monad.t =
      let path = Payload.path payload in
      let cond = Payload.cond payload in
      State.Monad.Monadic.return
        (Path.Subject.Test.transform_stm_list path ~target:test
           ~f:(wrap_in_if ~cond))
  end

  module Duplicate : S = Make (struct
    let name_suffix = "duplicate"

    let readme_suffix =
      {| This version of the action generates an arbitrary condition,
          and initialises both branches of the `if` statement with the
          original statements. |}

    let cond_env (vars : Var.Map.t) ~(tid : int) :
        (module Act_c_mini.Env_types.S) =
      Var.Map.env_module_satisfying_all ~scope:(Local tid) ~predicates:[]
        vars

    let cond_gen (path : Path_shapes.program) :
        Act_c_mini.Expression.t Base_quickcheck.Generator.t State.Monad.t =
      State.Monad.Let_syntax.(
        let%map vars = State.Monad.peek State.vars in
        let tid = Path.tid path in
        let (module Env) = cond_env vars ~tid in
        let module Bools = Act_c_mini.Expression_gen.Bool_values (Env) in
        Bools.quickcheck_generator)

    let t_branch_of_statements (statements : Subject.Statement.t list) :
        Subject.Block.t =
      Act_c_mini.Block.make ~statements ~metadata:Metadata.generated ()

    let f_branch_of_statements = t_branch_of_statements
  end)
end
