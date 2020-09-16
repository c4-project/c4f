(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("flow" @: "loop" @: rest)

module Insert = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fir.Expression.t Fuzz.Payload_impl.Insertion.t

  module While_false : S = struct
    let name =
      prefix_name Common.Id.("insert" @: "while" @: "false" @: empty)

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {| Inserts an empty while loop whose condition is known to be false,
          and whose body is marked as dead-code for future actions. |}

    let available : Fuzz.Availability.t = Fuzz.Availability.has_threads

    module Payload = Fuzz.Payload_impl.Insertion.Make (struct
      type t = Fir.Expression.t [@@deriving sexp]

      let path_filter _ = Fuzz.Path_filter.empty

      let gen ({path; _} : Fuzz.Path.Flagged.t) : t Fuzz.Payload_gen.t =
        let tid = Fuzz.Path.tid path in
        Fuzz.Payload_gen.(
          let* vars = vars in
          let env =
            Fuzz.Var.Map.env_satisfying_all vars ~scope:(Local tid)
              ~predicates:[]
          in
          lift_quickcheck (Fir_gen.Expr.falsehood env))
    end)

    let make_while (to_insert : Fir.Expression.t) : Fuzz.Subject.Statement.t
        =
      Accessor.construct Fir.Statement.flow
        Fir.Flow_block.(
          make
            ~header:(Header.While (While, to_insert))
            ~body:(Fuzz.Subject.Block.make_dead_code ()))

    (* TODO(@MattWindsor91): unify this with things? *)
    let run (subject : Fuzz.Subject.Test.t)
        ~(payload : Fir.Expression.t Fuzz.Payload_impl.Insertion.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      let to_insert = Fuzz.Payload_impl.Insertion.to_insert payload in
      let path = Fuzz.Payload_impl.Insertion.where payload in
      Fuzz.State.Monad.(
        (* NB: See discussion in Surround's apply function. *)
        add_expression_dependencies to_insert
          ~scope:(Local (Fuzz.Path.tid path.path))
        >>= fun () ->
        Monadic.return
          (Fuzz.Path_consumers.consume_with_flags subject ~path
             ~action:(Insert [make_while to_insert])))
  end
end

module Surround = struct
  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Fuzz.Payload_impl.Cond_surround.t

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

    val path_filter : Fuzz.Availability.Context.t -> Fuzz.Path_filter.t
    (** [path_filter ctx] generates the filter for the loop path. *)

    val cond_gen : Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t
    (** [cond_gen] generates the conditional for the loop. *)
  end) : S = Fuzz.Action.Make_surround (struct
    let name =
      prefix_name
        Common.Id.(
          "surround" @: Basic.kind_name @: Basic.name_suffix @: empty)

    let surround_with = Basic.kind_name ^ " loops"

    let readme_suffix = Basic.readme_suffix

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        M.(
          lift Basic.path_filter
          >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      include Fuzz.Payload_impl.Cond_surround.Make (struct
        let cond_gen = Basic.cond_gen

        let path_filter = Basic.path_filter
      end)

      let where = Fuzz.Payload_impl.Cond_surround.where

      let src_exprs x = [Fuzz.Payload_impl.Cond_surround.cond x]
    end

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      ignore payload ;
      Fuzz.State.Monad.return test

    let wrap (statements : Fuzz.Metadata.t Fir.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Metadata.t Fir.Statement.t =
      let cond = Fuzz.Payload_impl.Cond_surround.cond payload in
      Accessor.construct Fir.Statement.flow
        (Fir.Flow_block.while_loop ~kind:Basic.kind ~cond
           ~body:(Fuzz.Subject.Block.make_generated ~statements ()))
  end)

  let live_surround_path_filter _ : Fuzz.Path_filter.t =
    (* Don't surround breaks and continues in live code; doing so causes them
       to affect the new surrounding loop, which is a semantic change. *)
    Fuzz.Path_filter.(
      require_end_check empty
        ~check:
          (Stm_class
             ( Has_not_any
             , Fir.Statement_class.
                 [ Prim (Some (Early_out (Some Break)))
                 ; Prim (Some (Early_out (Some Continue))) ] )))

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

    let path_filter : Fuzz.Availability.Context.t -> Fuzz.Path_filter.t =
      live_surround_path_filter
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

    let path_filter (_ : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(in_dead_code_only empty)
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
      Fir_gen.Expr.bool

    let path_filter (_ : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(in_dead_code_only empty)
  end)

  module For = struct
    let prefix_name (x : Common.Id.t) : Common.Id.t =
      prefix_name Common.Id.("surround" @: "for" @: x)

    module Counter = struct
      type t = {var: Common.Litmus_id.t; ty: Fir.Type.t} [@@deriving sexp]

      let lc_expr ({var; ty} : t) : Fir.Expression.t =
        let name = Common.Litmus_id.variable_name var in
        let lc_tvar = Common.C_named.make ty ~name in
        Fir.Expression.lvalue (Fir.Lvalue.on_value_of_typed_id lc_tvar)

      let declare_and_register ({var; ty} : t) (test : Fuzz.Subject.Test.t) :
          Fuzz.Subject.Test.t Fuzz.State.Monad.t =
        Fuzz.State.Monad.Let_syntax.(
          let value = Fir.Constant.zero_of_type ty in
          let init = Fir.Initialiser.make ~ty ~value in
          let%bind test' =
            Fuzz.State.Monad.register_and_declare_var var init test
          in
          (* Note that this sets the known value of the loop counter to 0, so
             we have to erase it again; otherwise, the for loop's activity on
             the loop counter would be ignored by the rest of the fuzzer and
             lead to semantic violations. *)
          let%bind () = Fuzz.State.Monad.erase_var_value var in
          let%map () = Fuzz.State.Monad.add_dependency var in
          test')
    end

    module Simple_payload = struct
      type t =
        {lc: Counter.t; up_to: Fir.Constant.t; where: Fuzz.Path.Flagged.t}
      [@@deriving sexp, fields]

      let src_exprs (x : t) : Fir.Expression.t list =
        [ Fir.Expression.constant x.up_to
        ; (* The loop counter is effectively being read from as well as
             written to each iteration. *)
          Counter.lc_expr x.lc ]
    end

    module Simple :
      Fuzz.Action_types.S with type Payload.t = Simple_payload.t =
    Fuzz.Action.Make_surround (struct
      let name = prefix_name Common.Id.("simple" @: empty)

      let surround_with : string = "for-loops"

      let readme_suffix : string =
        {| The for loop initialises its (fresh) counter to zero, then counts
         upwards to a random, small, constant value.  This action does not
         surround non-generated or loop-unsafe statements. |}

      let path_filter (ctx : Fuzz.Availability.Context.t) =
        Fuzz.Path_filter.(
          require_end_check ~check:(Stm_no_meta_restriction Once_only)
          @@ live_surround_path_filter ctx)

      let available : Fuzz.Availability.t =
        Fuzz.Availability.(
          M.(
            lift path_filter >>= is_filter_constructible ~kind:Transform_list))

      module Payload = struct
        include Simple_payload

        let gen : t Fuzz.Payload_gen.t =
          Fuzz.Payload_gen.(
            let* filter =
              lift (Fn.compose path_filter Context.to_availability)
            in
            let* where = path_with_flags Transform_list ~filter in
            let scope = Common.Scope.Local (Fuzz.Path.tid where.path) in
            let* lc_var = fresh_var scope in
            let lc : Counter.t = {var= lc_var; ty= Fir.Type.int ()} in
            let+ up_to =
              lift_quickcheck
                Base_quickcheck.Generator.small_strictly_positive_int
            in
            {lc; up_to= Int up_to; where})
      end

      let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
          Fuzz.Subject.Test.t Fuzz.State.Monad.t =
        Counter.declare_and_register payload.lc test

      let control (payload : Payload.t) : Act_fir.Flow_block.For.t =
        Fir.Flow_block.For.make
          ~lvalue:
            (Accessor.construct Fir.Lvalue.variable
               (Common.Litmus_id.variable_name payload.lc.var))
          ~direction:Up_inclusive
          ~init_value:(Fir.Expression.int_lit 0)
          ~cmp_value:(Fir.Expression.constant payload.up_to)

      let wrap (statements : Fuzz.Subject.Statement.t list)
          ~(payload : Payload.t) : Fuzz.Subject.Statement.t =
        let control = control payload in
        let body = Fuzz.Subject.Block.make_generated ~statements () in
        Fir.(
          Accessor.construct Statement.flow
            (Flow_block.for_loop ~control ~body))
    end)

    module Kv_payload = struct
      type t =
        { lc: Counter.t
        ; kv_val: Fir.Constant.t
        ; kv_expr: Fir.Expression.t
        ; where: Fuzz.Path.Flagged.t }
      [@@deriving sexp, fields]

      let src_exprs (x : t) : Fir.Expression.t list =
        [ x.kv_expr
        ; (* The loop counter is effectively being read from as well as
             written to each iteration. *)
          Counter.lc_expr x.lc ]
    end

    module Kv_once : Fuzz.Action_types.S with type Payload.t = Kv_payload.t =
    Fuzz.Action.Make_surround (struct
      let name = prefix_name Common.Id.("once-kv" @: empty)

      let surround_with : string = "for-loops"

      let readme_suffix : string =
        {| The for loop initialises its
        (fresh) counter to the known value of an existing variable and
        compares it in such a way as to execute only once. |}

      let is_int : Fuzz.Var.Record.t -> bool =
        Fir.Type.Prim.eq ~to_:Int
          Accessor.(
            Fuzz.Var.Record.Access.type_of @> Fir.Type.Access.basic_type
            @> Fir.Type.Basic.Access.prim)

      let var_preds = [Fuzz.Var.Record.has_known_value; is_int]

      (* These may induce atomic loads, and so can't be in atomic blocks. *)
      let path_filter (ctx : Fuzz.Availability.Context.t) =
        Fuzz.Path_filter.(
          not_in_atomic_block
          @@ Fuzz.Availability.in_thread_with_variables ~predicates:var_preds
               ctx
          @@ live_surround_path_filter ctx)

      let available : Fuzz.Availability.t =
        Fuzz.Availability.(
          has_variables ~predicates:var_preds
          + M.(
              lift path_filter
              >>= is_filter_constructible ~kind:Transform_list))

      module Payload = struct
        include Kv_payload

        let access_of_var (vrec : Fir.Env.Record.t Common.C_named.t) :
            Fir.Expression.t Fuzz.Payload_gen.t =
          let var =
            Common.C_named.map_right
              ~f:(Accessor_base.get Fir.Env.Record.type_of)
              vrec
          in
          let ty = var.@(Common.C_named.value) in
          (* TODO(@MattWindsor91): is this duplicating something? *)
          Fuzz.Payload_gen.(
            if Fir.Type.is_atomic ty then
              let+ mo = lift_quickcheck Fir.Mem_order.gen_load in
              Fir.Expression.atomic_load
                (Fir.Atomic_load.make
                   ~src:(Fir.Address.on_address_of_typed_id var)
                   ~mo)
            else
              return
                (Fir.Expression.lvalue (Fir.Lvalue.on_value_of_typed_id var)))

        let known_value_val (vrec : Fir.Env.Record.t Common.C_named.t) :
            Fir.Constant.t Fuzz.Payload_gen.t =
          vrec.@?(Common.C_named.value @> Fir.Env.Record.known_value)
          |> Result.of_option
               ~error:
                 (Error.of_string "chosen value should have a known value")
          |> Fuzz.Payload_gen.Inner.return

        let known_value_var (scope : Common.Scope.t) :
            Fir.Env.Record.t Common.C_named.t Fuzz.Payload_gen.t =
          (* TODO(@MattWindsor91): this shares a lot of DNA with atomic_store
             redundant. *)
          Fuzz.Payload_gen.(
            let* vs = vars in
            let env =
              Fuzz.Var.Map.env_satisfying_all ~scope vs ~predicates:var_preds
            in
            lift_quickcheck (Fir.Env.gen_random_var_with_record env))

        let counter_type (vrec : Fir.Env.Record.t Common.C_named.t) :
            Fir.Type.t =
          Fir.(
            let prim =
              vrec.@(Common.C_named.value @> Env.Record.type_of
                     @> Type.Access.basic_type @> Type.Basic.Access.prim)
            in
            Type.make
              (Type.Basic.make prim ~is_atomic:false)
              ~is_pointer:false ~is_volatile:false)

        let gen : t Fuzz.Payload_gen.t =
          Fuzz.Payload_gen.(
            let* filter =
              lift (Fn.compose path_filter Context.to_availability)
            in
            let* where = path_with_flags Transform_list ~filter in
            let scope = Common.Scope.Local (Fuzz.Path.tid where.path) in
            let* lc_var = fresh_var scope in
            let* kv_var = known_value_var scope in
            let lc_type = counter_type kv_var in
            let lc : Counter.t = {var= lc_var; ty= lc_type} in
            let* kv_val = known_value_val kv_var in
            let+ kv_expr = access_of_var kv_var in
            {lc; kv_val; kv_expr; where})
      end

      let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
          Fuzz.Subject.Test.t Fuzz.State.Monad.t =
        Counter.declare_and_register payload.lc test

      let direction (payload : Payload.t) :
          Act_fir.Flow_block.For.Direction.t =
        (* Try to avoid overflows by only going up if we're negative. *)
        match Fir.Constant.as_int payload.kv_val with
        | Ok x when x < 0 ->
            Up_inclusive
        | _ ->
            Down_inclusive

      let control (payload : Payload.t) : Act_fir.Flow_block.For.t =
        Fir.Flow_block.For.make
          ~lvalue:
            (Accessor.construct Fir.Lvalue.variable
               (Common.Litmus_id.variable_name payload.lc.var))
          ~direction:(direction payload)
          ~init_value:(Fir.Expression.constant payload.kv_val)
          ~cmp_value:payload.kv_expr

      let wrap (statements : Fuzz.Subject.Statement.t list)
          ~(payload : Payload.t) : Fuzz.Subject.Statement.t =
        let control = control payload in
        let body = Fuzz.Subject.Block.make_generated ~statements () in
        Fir.(
          Accessor.construct Statement.flow
            (Flow_block.for_loop ~control ~body))
    end)
  end
end
