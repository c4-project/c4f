(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Surround = struct
  module Payload = struct
    (* The generation functions vary according to the specific action, and so
       appear in the functor below. *)
    type t = {cond: Act_c_mini.Expression.t; path: Path_shapes.program}
    [@@deriving make, sexp, fields]
  end

  module type S = Action_types.S with type Payload.t = Payload.t

  let readme_prelude : string =
    {| Removes a sublist of statements from the program, replacing them
        with an `if` statement containing some transformation of the
        removed statements. |}

  module Make (Basic : sig
    val name_suffix : string
    (** [name_suffix] should be a tag to add to the end of the action ID. *)

    val readme_suffix : string
    (** [readme_suffix] should be a string to add to the end of the action
        README. *)

    val t_branch_of_statements : Subject.Statement.t list -> Subject.Block.t
    (** [t_branch_of_statements stms] should construct the true-branch block
        given the original statement span [stms]. *)

    val f_branch_of_statements : Subject.Statement.t list -> Subject.Block.t
    (** [f_branch_of_statements stms] should construct the false-branch block
        given the original statement span [stms]. *)

    val cond_gen :
         (module Act_c_mini.Env_types.S_with_known_values)
      -> Act_c_mini.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given a first-class environment module [env]
        capturing the variables in scope at the point where the if statement
        is appearing, return a Quickcheck generator generating expressions
        over those variables. *)
  end) : S = struct
    include Basic

    let name = Act_common.Id.of_string_list ["flow"; "if"; name_suffix]

    let readme () =
      let raw = readme_prelude ^ "\n\n" ^ readme_suffix in
      Act_utils.My_string.format_for_readme raw

    module Payload = struct
      include Payload

      (* TODO(@MattWindsor91): the dependency flow from paths to conditions
         is a little hackneyed. It could do with being cleaned up a little,
         maybe. *)

      let quickcheck_path (test : Subject.Test.t) :
          Path_shapes.program Base_quickcheck.Generator.t =
        Option.value_exn (Path.Test.try_gen_transform_stm_list test)

      let gen_path (test : Subject.Test.t)
          ~(random : Splittable_random.State.t) :
          Path_shapes.program State.Monad.t =
        Action.lift_quickcheck (quickcheck_path test) ~random

      let cond_env (vars : Var.Map.t) ~(tid : int) :
          (module Act_c_mini.Env_types.S_with_known_values) =
        Var.Map.env_module_with_known_values ~scope:(Local tid)
          ~predicates:[] vars

      let quickcheck_cond (path : Path_shapes.program) :
          Act_c_mini.Expression.t Base_quickcheck.Generator.t State.Monad.t =
        State.Monad.Let_syntax.(
          let%map vars = State.Monad.peek State.vars in
          let tid = Path.tid path in
          let env = cond_env vars ~tid in
          Basic.cond_gen env)

      let gen_cond (path : Path_shapes.program)
          ~(random : Splittable_random.State.t) :
          Act_c_mini.Expression.t State.Monad.t =
        State.Monad.Let_syntax.(
          let%bind gen = quickcheck_cond path in
          Action.lift_quickcheck gen ~random)

      let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t) :
          Payload.t State.Monad.t =
        State.Monad.Let_syntax.(
          let%bind path = gen_path test ~random in
          let%map cond = gen_cond path ~random in
          Payload.make ~cond ~path)
    end

    let available (test : Subject.Test.t) : bool State.Monad.t =
      test |> Act_litmus.Test.Raw.threads
      |> List.exists ~f:Subject.Thread.has_statements
      |> State.Monad.return

    let wrap_in_if_raw (statements : Metadata.t Act_c_mini.Statement.t list)
        ~(cond : Act_c_mini.Expression.t) : Metadata.t Act_c_mini.Statement.t
        =
      Act_c_mini.Statement.if_stm
        (Act_c_mini.Statement.If.make ~cond
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements))

    let wrap_in_if (statements : Metadata.t Act_c_mini.Statement.t list)
        ~(cond : Act_c_mini.Expression.t) :
        Metadata.t Act_c_mini.Statement.t list Or_error.t =
      Or_error.return [wrap_in_if_raw statements ~cond]

    (** [add_cond_dependencies path cond] marks every variable in [cond] as
        having a read dependency, using [path] to resolve the scope at which
        [cond] is being inserted.

        This is a considerable over-approximation of the actual needed
        dependencies. *)
    let add_cond_dependencies (path : Path_shapes.program)
        (cond : Act_c_mini.Expression.t) : unit State.Monad.t =
      (* TODO(@MattWindsor91): it would be pretty cool for the lvalues to
         track whether or not they are being used in a tautology, once we add
         metadata tracking to the expression. In this case, we could skip
         adding dependencies where not necessary. *)
      State.Monad.add_expression_dependencies cond
        ~scope:(Local (Path.tid path))

    let run (test : Subject.Test.t) ~(payload : Payload.t) :
        Subject.Test.t State.Monad.t =
      let path = Payload.path payload in
      let cond = Payload.cond payload in
      State.Monad.(
        Let_syntax.(
          let%bind () = add_cond_dependencies path cond in
          Monadic.return
            (Path.Test.transform_stm_list path ~target:test
               ~f:(wrap_in_if ~cond))))
  end

  module Duplicate : S = Make (struct
    let name_suffix : string = "duplicate"

    let readme_suffix : string =
      {| This version of the action generates an arbitrary condition,
          and initialises both branches of the `if` statement with the
          original statements. |}

    let cond_gen (module Env : Act_c_mini.Env_types.S_with_known_values) :
        Act_c_mini.Expression.t Base_quickcheck.Generator.t =
      let module B = Act_c_mini.Expression_gen.Bool_values (Env) in
      B.quickcheck_generator

    let t_branch_of_statements (statements : Subject.Statement.t list) :
        Subject.Block.t =
      Subject.Block.make_generated ~statements ()

    let f_branch_of_statements : Subject.Statement.t list -> Subject.Block.t
        =
      t_branch_of_statements
  end)

  module Tautology : S = Make (struct
    let name_suffix : string = "tautology"

    let readme_suffix : string =
      {| This version of the action generates an always-true condition,
         puts the original statements in the true block, and marks the false
         block as dead-code. |}

    let cond_gen (module Env : Act_c_mini.Env_types.S_with_known_values) :
        Act_c_mini.Expression.t Base_quickcheck.Generator.t =
      let module B = Act_c_mini.Expression_gen.Bool_tautologies (Env) in
      B.quickcheck_generator

    let t_branch_of_statements (statements : Subject.Statement.t list) :
        Subject.Block.t =
      Subject.Block.make_generated ~statements ()

    let f_branch_of_statements (_statements : Subject.Statement.t list) :
        Subject.Block.t =
      Act_c_mini.Block.make ~metadata:Metadata.dead_code ()
  end)
end

module Invert : Action_types.S with type Payload.t = Path_shapes.program =
struct
  let name = Act_common.Id.of_string_list ["flow"; "invert-if"]

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Flips the conditional and branches of an if statement. |}

  let available (test : Subject.Test.t) : bool State.Monad.t =
    test |> Act_litmus.Test.Raw.threads
    |> List.exists ~f:Subject.Thread.has_if_statements
    |> State.Monad.return

  module Payload = struct
    type t = Path_shapes.program [@@deriving sexp]

    let quickcheck_path (test : Subject.Test.t) :
        Path_shapes.program Base_quickcheck.Generator.t =
      Option.value_exn
        (Path.Test.try_gen_transform_stm
           ~predicate:Act_c_mini.Statement.is_if_statement test)

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t) :
        Path_shapes.program State.Monad.t =
      Action.lift_quickcheck (quickcheck_path test) ~random
  end

  let invert_if (ifs : Metadata.t Act_c_mini.Statement.If.t) :
      Metadata.t Act_c_mini.Statement.If.t =
    Act_c_mini.Statement.If.(
      make
        ~cond:(Act_c_mini.Expression.l_not (cond ifs))
        ~t_branch:(f_branch ifs) (* intentional inversion *)
        ~f_branch:(t_branch ifs)
      (* as above *))

  let not_an_if (type leg) (stm : Subject.Statement.t) (_ : leg) :
      leg Or_error.t =
    Or_error.error_s
      [%message
        "Tried to if-invert an invalid statement"
          ~stm:(stm : Subject.Statement.t)]

  module Bm = Act_c_mini.Statement.Base_map (Or_error)

  let invert_stm (stm : Subject.Statement.t) : Subject.Statement.t Or_error.t
      =
    Bm.bmap stm ~assign:(not_an_if stm) ~atomic_cmpxchg:(not_an_if stm)
      ~atomic_store:(not_an_if stm) ~while_loop:(not_an_if stm)
      ~nop:(not_an_if stm)
      ~if_stm:(Fn.compose Or_error.return invert_if)

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path.Test.transform_stm payload ~target:test ~f:invert_stm)
end
