(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = Act_fir
  module F = Act_fuzz
end

module Insert = struct
  module type S =
    F.Action_types.S
      with type Payload.t = Fir.Expression.t F.Payload_impl.Insertion.t

  module While_false : S = struct
    let name =
      Act_common.Id.("flow" @: "loop" @: "while" @: "insert-false" @: empty)

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {| Inserts an empty while loop whose condition is known to be false,
          and whose body is marked as dead-code for future actions. |}

    let available : F.Availability.t = F.Availability.has_threads

    module Payload = F.Payload_impl.Insertion.Make (struct
      type t = Fir.Expression.t [@@deriving sexp]

      let path_filter _ = F.Path_filter.empty

      let gen (ins_path : F.Path.Test.t) : t F.Payload_gen.t =
        F.Payload_gen.(
          let* vars = vars in
          let tid = F.Path.Test.tid ins_path in
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
          ~scope:(Local (F.Path.Test.tid path))
        >>= fun () ->
        Monadic.return
          (F.Path_consumers.consume subject ~path
             ~action:(Insert [make_while to_insert])))
  end
end

module Surround = struct
  module type S =
    F.Action_types.S with type Payload.t = F.Payload_impl.Cond_surround.t

  module Do_false : S = struct
    let name = Act_common.Id.of_string_list ["flow"; "loop"; "surround"]

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {| Removes a sublist
        of statements from the program, replacing them with a `do... while`
        statement containing some transformation of the removed statements.

        The condition of the `do... while` loop is statically guaranteed to be
        false. |}

    let available (ctx : F.Availability.Context.t) : bool Or_error.t =
      Ok
        ( ctx |> F.Availability.Context.subject
        |> F.Subject.Test.has_statements )

    module Surround = F.Payload_impl.Cond_surround

    module Payload = Surround.Make (struct
      let cond_gen :
          Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t =
        Fir.Expression_gen.gen_falsehoods

      let path_filter (_ : F.Availability.Context.t) : F.Path_filter.t =
        F.Path_filter.empty
    end)

    let wrap_in_loop (cond : Fir.Expression.t)
        (statements : F.Metadata.t Fir.Statement.t list) :
        F.Metadata.t Fir.Statement.t =
      Fir.Statement.flow
        (Fir.Flow_block.while_loop ~kind:Do_while ~cond
           ~body:(F.Subject.Block.make_generated ~statements ()))

    let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      Surround.apply payload ~test ~f:wrap_in_loop
  end
end
