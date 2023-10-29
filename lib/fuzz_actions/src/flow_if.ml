(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t = Common.Id.("if" @: rest)

module Transform = struct
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Path.With_meta.t

  module Invert : S = struct
    let name = prefix_name Common.Id.("transform" @: "invert" @: empty)

    let readme =
      lazy {| Flips the conditional and branches of an if statement. |}

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(require_end_check (Stm_class (Is, [If])))

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible path_filter ~kind:Transform

    module Payload = struct
      type t = Fuzz.Path.With_meta.t [@@deriving sexp]

      let gen : Fuzz.Path.With_meta.t Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.path_with_flags Transform ~filter:path_filter
    end

    let recommendations (_ : Payload.t) : Common.Id.t list = []

    let invert_if ({cond; t_branch; f_branch} : Fuzz.Subject.Statement.If.t)
        : Fuzz.Subject.Statement.If.t =
      Fir.If.make
        ~cond:(Fir.Expression.l_not cond)
        ~t_branch:f_branch (* intentional inversion *)
        ~f_branch:t_branch

    (* as above *)

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
           ~action:(Transform invert_stm) )
  end
end

module Surround = struct
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Payload_impl.Cond_pathed.t

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
  end) : S = Fuzz.Action.Make_surround (struct
    let path_filter _ = Basic.path_filter

    let name =
      prefix_name Common.Id.("surround" @: Basic.name_suffix @: empty)

    let readme_suffix = Basic.readme_suffix

    let surround_with = "if statements"

    module Payload = struct
      (* TODO(@MattWindsor91): unify with Flow_while *)
      type t = Fir.Expression.t [@@deriving sexp]

      let src_exprs x = [x]

      let gen =
        Staged.unstage
          (Fuzz.Payload_impl.Cond_pathed.lift_cond_gen Basic.cond_gen)
    end

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      [Transform.Invert.name]

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible Basic.path_filter
        ~kind:Transform

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      ignore payload ;
      Fuzz.State.Monad.return test

    let wrap (statements : Fuzz.Subject.Statement.t list)
        ~(payload : Fir.Expression.t) : Fuzz.Subject.Statement.t =
      Accessor.construct Fir.Statement.if_stm
        (Fir.If.make ~cond:payload
           ~t_branch:(Basic.t_branch_of_statements statements)
           ~f_branch:(Basic.f_branch_of_statements statements) )
  end)

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
        require_end_check
          (Stm_class (Has_not_any, [Fir.Statement_class.label])) )
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
      Fir.Block.make ~metadata:Fuzz.Metadata.gen_once ~statements ()

    let f_branch_of_statements (_statements : Fuzz.Subject.Statement.t list)
        : Fuzz.Subject.Block.t =
      (* _statements contains the same statements as the true block; we want
         to generate an empty block, so we ignore it. *)
      Fir.Block.make ~metadata:Fuzz.Metadata.gen_dead ()

    let path_filter : Fuzz.Path_filter.t = Fuzz.Path_filter.zero
  end)
end
