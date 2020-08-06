(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module F = Act_fuzz
end

let prefix_name (rest : Ac.Id.t) : Ac.Id.t = Ac.Id.("flow" @: "if" @: rest)

module Surround = struct
  module type S =
    F.Action_types.S with type Payload.t = F.Payload_impl.Cond_surround.t

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
      F.Subject.Statement.t list -> F.Subject.Block.t
    (** [t_branch_of_statements stms] should construct the true-branch block
        given the original statement span [stms]. *)

    val f_branch_of_statements :
      F.Subject.Statement.t list -> F.Subject.Block.t
    (** [f_branch_of_statements stms] should construct the false-branch block
        given the original statement span [stms]. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen env] should, given an environment [env] capturing the
        variables in scope at the point where the if statement is appearing,
        return a Quickcheck generator generating expressions over those
        variables. *)

    val path_filter : F.Path_filter.t
    (** [path_filter] should apply any extra requirements on path filters. *)
  end) : S = struct
    include Basic

    let name = prefix_name Ac.Id.("surround" @: name_suffix @: empty)

    let readme () =
      let raw = readme_prelude ^ "\n\n" ^ readme_suffix in
      Act_utils.My_string.format_for_readme raw

    module Surround = F.Payload_impl.Cond_surround

    module Payload = Surround.Make (struct
      include Basic

      let path_filter _ = path_filter
    end)

    let available : F.Availability.t =
      F.Availability.is_filter_constructible Basic.path_filter
        ~kind:Transform

    let wrap_in_if (statements : F.Subject.Statement.t list)
        ~(cond : Fir.Expression.t) : F.Subject.Statement.t =
      Fir.Statement.if_stm
        (Fir.If.make ~cond
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements))

    let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
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
      Fir.Expression_gen.gen_bools

    let t_branch_of_statements (statements : F.Subject.Statement.t list) :
        F.Subject.Block.t =
      F.Subject.Block.make_generated ~statements ()

    let f_branch_of_statements :
        F.Subject.Statement.t list -> F.Subject.Block.t =
      t_branch_of_statements

    let path_filter : F.Path_filter.t =
      F.Path_filter.(
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
      Fir.Expression_gen.gen_tautologies

    let t_branch_of_statements (statements : F.Subject.Statement.t list) :
        F.Subject.Block.t =
      F.Subject.Block.make_generated ~statements ()

    let f_branch_of_statements (_statements : F.Subject.Statement.t list) :
        F.Subject.Block.t =
      Fir.Block.make ~metadata:F.Metadata.dead_code ()

    let path_filter : F.Path_filter.t = F.Path_filter.empty
  end)
end

module Transform = struct
  module type S = F.Action_types.S with type Payload.t = F.Path.t

  module Invert : S = struct
    let name = prefix_name Ac.Id.("transform" @: "invert" @: empty)

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Flips the conditional and branches of an if statement. |}

    let path_filter : F.Path_filter.t =
      F.Path_filter.(require_end_check empty ~check:(Stm_class (Is, [If])))

    let available : F.Availability.t =
      F.Availability.is_filter_constructible path_filter ~kind:Transform

    module Payload = struct
      type t = F.Path.t [@@deriving sexp]

      let gen : F.Path.t F.Payload_gen.t =
        F.Payload_gen.path Transform ~filter:path_filter
    end

    let invert_if (ifs : F.Subject.Statement.If.t) : F.Subject.Statement.If.t
        =
      Fir.If.(
        make
          ~cond:(Fir.Expression.l_not (cond ifs))
          ~t_branch:(f_branch ifs) (* intentional inversion *)
          ~f_branch:(t_branch ifs)
        (* as above *))

    let not_an_if (type leg) (stm : F.Subject.Statement.t) (_ : leg) :
        leg Or_error.t =
      Or_error.error_s
        [%message
          "Tried to if-invert an invalid statement"
            ~stm:(stm : F.Subject.Statement.t)]

    module Bm = Fir.Statement_traverse.Base_map (Or_error)

    let invert_stm (stm : F.Subject.Statement.t) :
        F.Subject.Statement.t Or_error.t =
      Bm.bmap stm ~prim:(not_an_if stm) ~flow:(not_an_if stm)
        ~if_stm:(Fn.compose Or_error.return invert_if)

    let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      F.State.Monad.Monadic.return
        (F.Path_consumers.consume test ~filter:path_filter ~path:payload
           ~action:(Transform invert_stm))
  end
end
