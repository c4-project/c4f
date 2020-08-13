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
  end) : S = F.Action.Make_surround (struct
    let name =
      prefix_name
        Ac.Id.("surround" @: Basic.kind_name @: Basic.name_suffix @: empty)

    let surround_with = Basic.kind_name ^ " loops"

    let readme_suffix = Basic.readme_suffix

    let available : F.Availability.t =
      F.Availability.(
        M.(
          lift Basic.path_filter
          >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      include F.Payload_impl.Cond_surround.Make (struct
        let cond_gen = Basic.cond_gen

        let path_filter = Basic.path_filter
      end)

      let where = F.Payload_impl.Cond_surround.where

      let src_exprs x = [F.Payload_impl.Cond_surround.cond x]
    end

    let run_pre (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      ignore payload ; F.State.Monad.return test

    let wrap (statements : F.Metadata.t Fir.Statement.t list)
        ~(payload : Payload.t) : F.Metadata.t Fir.Statement.t =
      let cond = F.Payload_impl.Cond_surround.cond payload in
      Fir.Statement.flow
        (Fir.Flow_block.while_loop ~kind:Basic.kind ~cond
           ~body:(F.Subject.Block.make_generated ~statements ()))
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

  module For_kv_payload = struct
    type t =
      { lc_var: Ac.Litmus_id.t
      ; lc_type: Fir.Type.t
      ; kv_val: Fir.Constant.t
      ; kv_expr: Fir.Expression.t
      ; where: F.Path.Flagged.t }
    [@@deriving sexp, fields]

    let lc_expr (x : t) : Fir.Expression.t =
      let name = Ac.Litmus_id.variable_name x.lc_var in
      let lc_tvar = Ac.C_named.make x.lc_type ~name in
      Fir.Expression.lvalue (Fir.Lvalue.on_value_of_typed_id lc_tvar)

    let src_exprs (x : t) : Fir.Expression.t list = [
      x.kv_expr;
      (* The loop counter is effectively being read from as well as written to
         each iteration. *)
      lc_expr x]
  end

  module For_kv_once :
    F.Action_types.S with type Payload.t = For_kv_payload.t =
  F.Action.Make_surround (struct
    let name = prefix_name Ac.Id.("surround" @: "for" @: "once-kv" @: empty)

    let surround_with : string = "for-loops"

    let readme_suffix : string =
      {| The for loop initialises its
        (fresh) counter to the known value of an existing variable and
        compares it in such a way as to execute only once. |}

    let is_int : F.Var.Record.t -> bool =
      Fir.Type.Prim.eq ~to_:Int
      Accessor.(
        F.Var.Record.Access.ty @> Fir.Type.Access.basic_type @> Fir.Type.Basic.Access.prim
      )

    let var_preds = [F.Var.Record.has_known_value; is_int]

    (* These may induce atomic loads, and so can't be in atomic blocks. *)
    let path_filter (ctx : F.Availability.Context.t) =
      F.Path_filter.(
        not_in_atomic_block
        @@ F.Availability.in_thread_with_variables ~predicates:var_preds ctx
        @@ empty)

    let available : F.Availability.t =
      F.Availability.(
        has_variables ~predicates:var_preds
        + M.(
            lift path_filter >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      include For_kv_payload

      let access_of_var (vrec : Fir.Env.Record.t Ac.C_named.t) :
          Fir.Expression.t F.Payload_gen.t =
        let var = Ac.C_named.map_right ~f:Fir.Env.Record.type_of vrec in
        let ty = Ac.C_named.value var in
        (* TODO(@MattWindsor91): is this duplicating something? *)
        F.Payload_gen.(
          if Fir.Type.is_atomic ty then
            let+ mo = lift_quickcheck Fir.Mem_order.gen_load in
            Fir.Expression.atomic_load
              (Fir.Atomic_load.make
                 ~src:(Fir.Address.on_address_of_typed_id var)
                 ~mo)
          else
            return
              (Fir.Expression.lvalue (Fir.Lvalue.on_value_of_typed_id var)))

      let known_value_val (vrec : Fir.Env.Record.t Ac.C_named.t) :
          Fir.Constant.t F.Payload_gen.t =
        vrec |> Ac.C_named.value |> Fir.Env.Record.known_value
        |> Result.of_option
             ~error:
               (Error.of_string "chosen value should have a known value")
        |> F.Payload_gen.Inner.return

      let known_value_var (scope : Ac.Scope.t) :
          Fir.Env.Record.t Ac.C_named.t F.Payload_gen.t =
        (* TODO(@MattWindsor91): this shares a lot of DNA with atomic_store
           redundant. *)
        F.Payload_gen.(
          let* vs = vars in
          let env =
            F.Var.Map.env_satisfying_all ~scope vs ~predicates:var_preds
          in
          lift_quickcheck (Fir.Env.gen_random_var_with_record env))

      let counter_type (vrec : Fir.Env.Record.t Ac.C_named.t) : Fir.Type.t =
        vrec |> Ac.C_named.value |> Fir.Env.Record.type_of
        |> Fir.Type.strip_atomic |> Fir.Type.basic_type
        |> Fir.Type.make ~is_pointer:false ~is_volatile:false

      let gen : t F.Payload_gen.t =
        F.Payload_gen.(
          let* filter =
            lift (Fn.compose path_filter Context.to_availability)
          in
          let* where = path_with_flags Transform_list ~filter in
          let scope =
            Ac.Scope.Local (F.Path.tid (F.Path_flag.Flagged.path where))
          in
          let* lc_var = fresh_var scope in
          let* kv_var = known_value_var scope in
          let lc_type = counter_type kv_var in
          let* kv_val = known_value_val kv_var in
          let+ kv_expr = access_of_var kv_var in
          {lc_var; lc_type; kv_val; kv_expr; where})
    end

    let run_pre (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      let init = Fir.Initialiser.make ~ty:payload.lc_type () in
      F.State.Monad.(
        register_var payload.lc_var init
        >>= fun () ->
        Monadic.return (F.Subject.Test.declare_var test payload.lc_var init))

    let direction (payload : Payload.t) : Act_fir.Flow_block.For.Direction.t
        =
      (* Try to avoid overflows by only going up if we're negative. *)
      match Fir.Constant.as_int payload.kv_val with
      | Ok x when x < 0 ->
          Up_inclusive
      | _ ->
          Down_inclusive

    let control (payload : Payload.t) : Act_fir.Flow_block.For.t =
      Fir.Flow_block.For.make
        ~lvalue:
          (Fir.Lvalue.variable (Ac.Litmus_id.variable_name payload.lc_var))
        ~direction:(direction payload)
        ~init_value:(Fir.Expression.constant payload.kv_val)
        ~cmp_value:payload.kv_expr

    let wrap (statements : F.Subject.Statement.t list) ~(payload : Payload.t)
        : F.Subject.Statement.t =
      let control = control payload in
      let body = F.Subject.Block.make_generated ~statements () in
      Fir.(Statement.flow (Flow_block.for_loop ~control ~body))
  end)
end
