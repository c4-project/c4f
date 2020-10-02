(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  (* Shared with Flow_for. *)
  Common.Id.("loop" @: rest)

module Insert = struct
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Payload_impl.Cond_pathed.t

  module False : S = struct
    let name =
      prefix_name Common.Id.("insert" @: "while" @: "false" @: empty)

    let readme : string Lazy.t =
      lazy
        {| Inserts an empty while loop whose condition is known to be false,
        and whose body is marked as dead-code for future actions. |}

    let available : Fuzz.Availability.t = Fuzz.Availability.has_threads

    module Payload = struct
      type t = Fir.Expression.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

      let path_filter _ = Fuzz.Path_filter.zero

      let gen' ({path; _} : Fuzz.Path.Flagged.t) :
          Fir.Expression.t Fuzz.Payload_gen.t =
        let tid = Fuzz.Path.tid path in
        Fuzz.Payload_gen.(
          let* vars = vars in
          let env =
            Fuzz.Var.Map.env_satisfying_all vars ~scope:(Local tid)
              ~predicates:[]
          in
          lift_quickcheck (Fir_gen.Expr.falsehood env))

      let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
    end

    let recommendations (_ : Fuzz.Payload_impl.Cond_pathed.t) :
        Common.Id.t list =
      (* No point introducing dead-code actions if we're introducing dead
         code. *)
      []

    let make_while (to_insert : Fir.Expression.t) : Fuzz.Subject.Statement.t
        =
      Accessor.construct Fir.Statement.flow
        Fir.Flow_block.(
          make
            ~header:(Header.While (While, to_insert))
            ~body:(Fuzz.Subject.Block.make_dead_code ()))

    (* TODO(@MattWindsor91): unify this with things? *)
    let run (test : Fuzz.Subject.Test.t)
        ~(payload : Fir.Expression.t Fuzz.Payload_impl.Pathed.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.Payload_impl.Cond_pathed.insert payload ~test ~f:make_while
  end
end

module Surround = struct
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Payload_impl.Cond_pathed.t

  module Make (Basic : sig
    val kind : Fir.Flow_block.While.t
    (** [kind] is the kind of loop to make. *)

    val kind_name : string
    (** [kind_name] is the name of the kind of loop to make, as it should
        appear in the identifier. *)

    val name_suffix : string
    (** [name_suffix] becomes the last tag of the action name. *)

    val readme_suffix : string
    (** [readme_suffix] gets appended onto the end of the readme. *)

    val path_filter : Fuzz.Path_filter.t
    (** [path_filter ctx] generates the filter for the loop path. *)

    val recommendations : Fuzz.Payload_impl.Cond_pathed.t -> Common.Id.t list
    (** [recommendations p] gets the action recommendations given [p]. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen] generates the conditional for the loop. *)
  end) : S = Fuzz.Action.Make_surround (struct
    let name =
      prefix_name
        Common.Id.(
          "surround" @: Basic.kind_name @: Basic.name_suffix @: empty)

    let surround_with = Basic.kind_name ^ " loops"

    let readme_suffix = Basic.readme_suffix

    let path_filter _ = Basic.path_filter

    let recommendations = Basic.recommendations

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        M.(lift path_filter >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      type t = Fir.Expression.t [@@deriving sexp]

      let src_exprs x = [x]

      let gen =
        Staged.unstage
          (Fuzz.Payload_impl.Cond_pathed.lift_cond_gen Basic.cond_gen)
    end

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      ignore payload ;
      Fuzz.State.Monad.return test

    let wrap (statements : Fuzz.Metadata.t Fir.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Metadata.t Fir.Statement.t =
      let cond = payload in
      Accessor.construct Fir.Statement.flow
        (Fir.Flow_block.while_loop ~kind:Basic.kind ~cond
           ~body:(Fuzz.Subject.Block.make_generated ~statements ()))
  end)

  module Do_false : S = Make (struct
    let kind = Fir.Flow_block.While.Do_while

    let kind_name = "do"

    let name_suffix = "false"

    let readme_suffix =
      {| The condition of the `do... while` loop is statically guaranteed to be
        false, meaning the loop will iterate only once. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir_gen.Expr.falsehood

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.live_loop_surround

    let recommendations (_ : Fuzz.Payload_impl.Cond_pathed.t) :
        Common.Id.t list =
      [Flow_dead.Insert.Early_out_loop_end.name]
  end)

  module Do_dead : S = Make (struct
    let kind = Fir.Flow_block.While.Do_while

    let kind_name = "do"

    let name_suffix = "dead"

    let readme_suffix =
      {| This action will only surround portions of dead code, but the condition
        of the `do... while` loop can be anything. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir_gen.Expr.bool

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.require_flag In_dead_code

    let recommendations (_ : Fuzz.Payload_impl.Cond_pathed.t) :
        Common.Id.t list =
      (* No point introducing dead-code actions if we're surrounding dead
         code. *)
      []
  end)

  module Dead : S = Make (struct
    let kind = Fir.Flow_block.While.While

    let kind_name = "while"

    let name_suffix = "dead"

    let readme_suffix =
      {| This action will only surround portions of dead code, but the condition
        of the `while` loop can be anything. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir_gen.Expr.bool

    let path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.require_flag In_dead_code

    let recommendations (_ : Fuzz.Payload_impl.Cond_pathed.t) :
        Common.Id.t list =
      (* No point introducing dead-code actions if we're surrounding dead
         code. *)
      []
  end)
end
