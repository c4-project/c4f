(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

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

let prefix_name (rest : Ac.Id.t) : Ac.Id.t = Ac.Id.("flow" @: "loop" @: rest)

module Insert = struct
  module type S =
    F.Action_types.S
      with type Payload.t = Fir.Expression.t F.Payload_impl.Insertion.t

  module While_false : S = struct
    let name = prefix_name Ac.Id.("insert" @: "while" @: "false" @: empty)

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {| Inserts an empty while loop whose condition is known to be false,
          and whose body is marked as dead-code for future actions. |}

    let available : F.Availability.t = F.Availability.has_threads

    module Payload = F.Payload_impl.Insertion.Make (struct
      type t = Fir.Expression.t [@@deriving sexp]

      let path_filter _ = F.Path_filter.empty

      let gen (ins_path : F.Path.Flagged.t) : t F.Payload_gen.t =
        F.Payload_gen.(
          let* vars = vars in
          let tid = F.Path.tid (F.Path_flag.Flagged.path ins_path) in
          let env =
            F.Var.Map.env_satisfying_all vars ~scope:(Local tid)
              ~predicates:[]
          in
          lift_quickcheck (Fir.Expression_gen.gen_falsehoods env))
    end)

    let make_while (to_insert : Fir.Expression.t) : F.Subject.Statement.t =
      Fir.Statement.flow
        Fir.Flow_block.(
          make
            ~header:(Header.While (While, to_insert))
            ~body:(F.Subject.Block.make_dead_code ()))

    (* TODO(@MattWindsor91): unify this with things? *)
    let run (subject : F.Subject.Test.t)
        ~(payload : Fir.Expression.t F.Payload_impl.Insertion.t) :
        F.Subject.Test.t F.State.Monad.t =
      let to_insert = F.Payload_impl.Insertion.to_insert payload in
      let path = F.Payload_impl.Insertion.where payload in
      F.State.Monad.(
        (* NB: See discussion in Surround's apply function. *)
        add_expression_dependencies to_insert
          ~scope:(Local (F.Path.tid (F.Path_flag.Flagged.path path)))
        >>= fun () ->
        Monadic.return
          (F.Path_consumers.consume_with_flags subject ~path
             ~action:(Insert [make_while to_insert])))
  end
end

module Surround = struct
  module type S =
    F.Action_types.S with type Payload.t = F.Payload_impl.Cond_surround.t

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

    val path_filter : F.Availability.Context.t -> F.Path_filter.t
    (** [path_filter ctx] generates the filter for the loop path. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen] generates the conditional for the loop. *)
  end) : S = struct
    let name =
      prefix_name
        Ac.Id.("surround" @: Basic.kind_name @: Basic.name_suffix @: empty)

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {| Removes a sublist
        of statements from the program, replacing them with a |}
      ^ Basic.kind_name
      ^ {| 
        statement containing some transformation of the removed statements.

        |}
      ^ Basic.readme_suffix

    let available : F.Availability.t =
      F.Availability.(
        has_threads
        + with_context (fun ctx ->
              is_filter_constructible (Basic.path_filter ctx)
                ~kind:Transform_list))

    module Surround = F.Payload_impl.Cond_surround

    module Payload = Surround.Make (struct
      let cond_gen = Basic.cond_gen

      let path_filter = Basic.path_filter
    end)

    let wrap_in_loop (cond : Fir.Expression.t)
        (statements : F.Metadata.t Fir.Statement.t list) :
        F.Metadata.t Fir.Statement.t =
      Fir.Statement.flow
        (Fir.Flow_block.while_loop ~kind:Basic.kind ~cond
           ~body:(F.Subject.Block.make_generated ~statements ()))

    let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      Surround.apply payload ~test ~f:wrap_in_loop
  end

  module Do_false : S = Make (struct
    let kind = Fir.Flow_block.While.Do_while

    let kind_name = "do"

    let name_suffix = "false"

    let readme_suffix =
      {| The condition of the `do... while` loop is statically guaranteed to be
         false, meaning the loop will iterate only once. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir.Expression_gen.gen_falsehoods

    let path_filter (_ : F.Availability.Context.t) : F.Path_filter.t =
      F.Path_filter.empty
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
      Fir.Expression_gen.gen_bools

    let path_filter (_ : F.Availability.Context.t) : F.Path_filter.t =
      F.Path_filter.(in_dead_code_only empty)
  end)

  module While_dead : S = Make (struct
    let kind = Fir.Flow_block.While.While

    let kind_name = "while"

    let name_suffix = "dead"

    let readme_suffix =
      {| This action will only surround portions of dead code, but the condition
         of the `while` loop can be anything. |}

    let cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
        =
      Fir.Expression_gen.gen_bools

    let path_filter (_ : F.Availability.Context.t) : F.Path_filter.t =
      F.Path_filter.(in_dead_code_only empty)
  end)
end
