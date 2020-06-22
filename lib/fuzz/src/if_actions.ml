(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

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
      Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given an environment [env] capturing the
        variables in scope at the point where the if statement is appearing,
        return a Quickcheck generator generating expressions over those
        variables. *)

    val path_filter : Path_filter.t State.Monad.t
    (** [path_filter] should apply any extra requirements on path filters. *)
  end) : S = struct
    include Basic

    let name = Act_common.Id.of_string_list ["flow"; "if"; name_suffix]

    let readme () =
      let raw = readme_prelude ^ "\n\n" ^ readme_suffix in
      Act_utils.My_string.format_for_readme raw

    module Surround = Payload.Surround

    module Payload = Surround.Make (struct
      include Basic

      let action_id = name
    end)

    let available (test : Subject.Test.t) ~(param_map : Param_map.t) :
        bool State.Monad.t =
      ignore (param_map : Param_map.t) ;
      test |> Subject.Test.has_statements |> State.Monad.return

    let wrap_in_if (statements : Metadata.t Act_fir.Statement.t list)
        ~(cond : Act_fir.Expression.t) : Metadata.t Act_fir.Statement.t =
      Act_fir.Statement.if_stm
        (Act_fir.If.make ~cond
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
          original statements.  It cannot fire if the statements contain
          any labels, to avoid duplicating them. |}

    let cond_gen :
        Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t =
      Act_fir.Expression_gen.gen_bools

    let t_branch_of_statements (statements : Subject.Statement.t list) :
        Subject.Block.t =
      Subject.Block.make_generated ~statements ()

    let f_branch_of_statements : Subject.Statement.t list -> Subject.Block.t
        =
      t_branch_of_statements

    let path_filter : Path_filter.t State.Monad.t =
      State.Monad.return
        Path_filter.(require_end_check empty ~check:Has_no_labels)
  end)

  module Tautology : S = Make (struct
    let name_suffix : string = "tautology"

    let readme_suffix : string =
      {| This version of the action generates an always-true condition,
         puts the original statements in the true block, and marks the false
         block as dead-code. |}

    let cond_gen :
        Act_fir.Env.t -> Act_fir.Expression.t Base_quickcheck.Generator.t =
      Act_fir.Expression_gen.gen_tautologies

    let t_branch_of_statements (statements : Subject.Statement.t list) :
        Subject.Block.t =
      Subject.Block.make_generated ~statements ()

    let f_branch_of_statements (_statements : Subject.Statement.t list) :
        Subject.Block.t =
      Act_fir.Block.make ~metadata:Metadata.dead_code ()

    let path_filter : Path_filter.t State.Monad.t =
      State.Monad.return Path_filter.empty
  end)
end

module Invert : Action_types.S with type Payload.t = Path.Program.t = struct
  let name = Act_common.Id.of_string_list ["flow"; "invert-if"]

  let readme () =
    Act_utils.My_string.format_for_readme
      {| Flips the conditional and branches of an if statement. |}

  let available (test : Subject.Test.t) ~(param_map : Param_map.t) :
      bool State.Monad.t =
    ignore (param_map : Param_map.t) ;
    test |> Subject.Test.has_if_statements |> State.Monad.return

  module Payload = struct
    type t = Path.Program.t [@@deriving sexp]

    let quickcheck_path (test : Subject.Test.t) : Path.Program.t Opt_gen.t =
      let filter =
        Path_filter.(empty |> require_end_check ~check:(Is_of_class If))
      in
      Path_producers.Test.try_gen_transform_stm ~filter test

    let gen (test : Subject.Test.t) ~(random : Splittable_random.State.t)
        ~(param_map : Param_map.t) : Path.Program.t State.Monad.t =
      ignore (param_map : Param_map.t) ;
      Payload.Helpers.lift_quickcheck_opt (quickcheck_path test) ~random
        ~action_id:name
  end

  let invert_if (ifs : Metadata.t Act_fir.Statement.If.t) :
      Metadata.t Act_fir.Statement.If.t =
    Act_fir.If.(
      make
        ~cond:(Act_fir.Expression.l_not (cond ifs))
        ~t_branch:(f_branch ifs) (* intentional inversion *)
        ~f_branch:(t_branch ifs)
      (* as above *))

  let not_an_if (type leg) (stm : Subject.Statement.t) (_ : leg) :
      leg Or_error.t =
    Or_error.error_s
      [%message
        "Tried to if-invert an invalid statement"
          ~stm:(stm : Subject.Statement.t)]

  module Bm = Act_fir.Statement_traverse.Base_map (Or_error)

  let invert_stm (stm : Subject.Statement.t) : Subject.Statement.t Or_error.t
      =
    Bm.bmap stm ~prim:(not_an_if stm) ~flow:(not_an_if stm)
      ~if_stm:(Fn.compose Or_error.return invert_if)

  let run (test : Subject.Test.t) ~(payload : Payload.t) :
      Subject.Test.t State.Monad.t =
    State.Monad.Monadic.return
      (Path_consumers.Test.transform_stm payload ~target:test ~f:invert_stm)
end
