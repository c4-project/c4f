(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Surround = struct
  (* This is shadowed, so we need to alias it. *)
  module Helpers = Payload.Helpers

  module type S = Action_types.S with type Payload.t = Payload.Surround.t

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

    module Surround = Payload.Surround
    module Payload = Surround.Make (Basic)

    let available (test : Subject.Test.t) : bool State.Monad.t =
      test |> Subject.Test.has_statements |> State.Monad.return

    let wrap_in_if (statements : Metadata.t Act_c_mini.Statement.t list)
        ~(cond : Act_c_mini.Expression.t) : Metadata.t Act_c_mini.Statement.t
        =
      Act_c_mini.Statement.if_stm
        (Act_c_mini.Statement.If.make ~cond
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements))

    let run (test : Subject.Test.t) ~(payload : Payload.t) :
        Subject.Test.t State.Monad.t =
      Surround.apply payload ~test ~f:(fun cond -> wrap_in_if ~cond)
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

module Invert : Action_types.S with type Payload.t = Path.program = struct
  let name = Act_common.Id.of_string_list ["flow"; "invert-if"]

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Flips the conditional and branches of an if statement. |}

  let available (test : Subject.Test.t) : bool State.Monad.t =
    test |> Subject.Test.has_if_statements |> State.Monad.return

  module Payload = struct
    type t = Path.program [@@deriving sexp]

    let quickcheck_path (test : Subject.Test.t) :
        Path.program Base_quickcheck.Generator.t option =
      let filter = Path_filter.(empty |> final_if_statements_only) in
      Path_producers.Test.try_gen_transform_stm ~filter test

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t) :
        Path.program State.Monad.t =
      Payload.Helpers.lift_quickcheck_opt (quickcheck_path test) ~random
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
    Bm.bmap stm ~prim:(not_an_if stm) ~while_loop:(not_an_if stm)
      ~if_stm:(Fn.compose Or_error.return invert_if)

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.Test.transform_stm payload ~target:test ~f:invert_stm)
end
