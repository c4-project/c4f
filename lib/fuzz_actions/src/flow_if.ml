(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("flow" @: "if" @: rest)

module Surround = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fuzz.Payload_impl.Cond_surround.t

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

    val t_branch_of_statements :
      Fuzz.Subject.Statement.t list -> Fuzz.Subject.Block.t
    (** [t_branch_of_statements stms] should construct the true-branch block
        given the original statement span [stms]. *)

    val f_branch_of_statements :
      Fuzz.Subject.Statement.t list -> Fuzz.Subject.Block.t
    (** [f_branch_of_statements stms] should construct the false-branch block
        given the original statement span [stms]. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given an environment [env] capturing the
        variables in scope at the point where the if statement is appearing,
        return a Quickcheck generator generating expressions over those
        variables. *)

    val path_filter : Fuzz.Path_filter.t
    (** [path_filter] should apply any extra requirements on path filters. *)
  end) : S = struct
    include Basic

    let name = prefix_name Common.Id.("surround" @: name_suffix @: empty)

    let readme () =
      let raw = readme_prelude ^ "\n\n" ^ readme_suffix in
      Act_utils.My_string.format_for_readme raw

    module Surround = Fuzz.Payload_impl.Cond_surround

    module Payload = Surround.Make (struct
      include Basic

      let path_filter _ = path_filter
    end)

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible Basic.path_filter
        ~kind:Transform

    let wrap_in_if (statements : Fuzz.Subject.Statement.t list)
        ~(cond : Fir.Expression.t) : Fuzz.Subject.Statement.t =
      Accessor.construct Fir.Statement.if_stm
        (Fir.If.make ~cond
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements))

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Surround.apply ~filter:path_filter payload ~test ~f:(fun cond ->
          wrap_in_if ~cond)
  end

  module Duplicate : S = Make (struct
    let name_suffix : string = "duplicate"

    let readme_suffix : string =
      {| This version of the action generates an arbitrary condition,
          and initialises both branches of the `if` statement with the
          original statements.  It cannot fire if the statements contain
          any labels, to avoid duplicating them. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir_gen.Expr.bool

    let t_branch_of_statements (statements : Fuzz.Subject.Statement.t list) :
        Fuzz.Subject.Block.t =
      Fuzz.Subject.Block.make_generated ~statements ()

    let f_branch_of_statements :
        Fuzz.Subject.Statement.t list -> Fuzz.Subject.Block.t =
      t_branch_of_statements

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        require_end_check empty
          ~check:(Stm_class (Has_not_any, [Fir.Statement_class.label])))
  end)

  module Tautology : S = Make (struct
    let name_suffix : string = "tautology"

    let readme_suffix : string =
      {| This version of the action generates an always-true condition,
         puts the original statements in the true block, and marks the false
         block as dead-code. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir_gen.Expr.tautology

    let t_branch_of_statements (statements : Fuzz.Subject.Statement.t list) :
        Fuzz.Subject.Block.t =
      Fuzz.Subject.Block.make_generated ~statements ()

    let f_branch_of_statements (_statements : Fuzz.Subject.Statement.t list)
        : Fuzz.Subject.Block.t =
      Fir.Block.make ~metadata:Fuzz.Metadata.dead_code ()

    let path_filter : Fuzz.Path_filter.t = Fuzz.Path_filter.empty
  end)
end

module Transform = struct
  module type S = Fuzz.Action_types.S with type Payload.t = Fuzz.Path.t

  module Invert : S = struct
    let name = prefix_name Common.Id.("transform" @: "invert" @: empty)

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Flips the conditional and branches of an if statement. |}

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        require_end_check empty ~check:(Stm_class (Is, [If])))

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible path_filter ~kind:Transform

    module Payload = struct
      type t = Fuzz.Path.t [@@deriving sexp]

      let gen : Fuzz.Path.t Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.path Transform ~filter:path_filter
    end

    let invert_if (ifs : Fuzz.Subject.Statement.If.t) :
        Fuzz.Subject.Statement.If.t =
      Fir.If.(
        make
          ~cond:(Fir.Expression.l_not (cond ifs))
          ~t_branch:(f_branch ifs) (* intentional inversion *)
          ~f_branch:(t_branch ifs)
        (* as above *))

    let not_an_if (type leg) (stm : Fuzz.Subject.Statement.t) (_ : leg) :
        leg Or_error.t =
      Or_error.error_s
        [%message
          "Tried to if-invert an invalid statement"
            ~stm:(stm : Fuzz.Subject.Statement.t)]

    module Bm = Fir.Statement_traverse.Base_map (Or_error)

    let invert_stm (stm : Fuzz.Subject.Statement.t) :
        Fuzz.Subject.Statement.t Or_error.t =
      Bm.bmap stm ~prim:(not_an_if stm) ~flow:(not_an_if stm)
        ~if_stm:(Fn.compose Or_error.return invert_if)

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.Monadic.return
        (Fuzz.Path_consumers.consume test ~filter:path_filter ~path:payload
           ~action:(Transform invert_stm))
  end
end
