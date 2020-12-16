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
  (* Shared with Flow_while. *)
  Common.Id.("loop" @: rest)

let is_int : Fuzz.Var.Record.t -> bool =
  (* TODO(@MattWindsor91): unify with the various 'has_variables_with' env
     stuff, somehow? *)
  Fir.Type.Prim.eq ~to_:Int
    Accessor.(
      Fuzz.Var.Record.Access.type_of @> Fir.Type.Access.basic_type
      @> Fir.Type.Basic.Access.prim)

let kv_var_preds = [Fuzz.Var.Record.has_known_value; is_int]

let kv_live_path_filter_static : Fuzz.Path_filter.t =
  Fuzz.Path_filter.(live_loop_surround + forbid_flag In_atomic)

let kv_live_path_filter ({vars; _} : Fuzz.State.t) : Fuzz.Path_filter.t =
  Fuzz.Path_filter.(
    (* These may induce atomic loads, and so can't be in atomic blocks. *)
    in_thread_with_variables ~predicates:kv_var_preds vars
    + kv_live_path_filter_static)

let kv_available (kind : Fuzz.Path_kind.t) : Fuzz.Availability.t =
  (* TODO(@MattWindsor91): is the has-variables redundant here? *)
  Fuzz.Availability.(
    has_variables ~predicates:kv_var_preds
    + M.(
        lift (fun {state; _} -> kv_live_path_filter state)
        >>= is_filter_constructible ~kind))

module Payload = struct
  module Counter = struct
    type t = {var: Common.Litmus_id.t; ty: Fir.Type.t} [@@deriving sexp]

    let can_make : Fuzz.Availability.t =
      Fuzz.Availability.in_var_cap ~after_adding:1

    let lc_expr ({var; ty} : t) : Fir.Expression.t =
      let name = Common.Litmus_id.variable_name var in
      let lc_tvar = Common.C_named.make ty ~name in
      Fir.Expression.lvalue (Fir.Lvalue.on_value_of_typed_id lc_tvar)

    let declare_and_register ({var; ty} : t) (test : Fuzz.Subject.Test.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.Let_syntax.(
        let value = Fir.Constant.zero_of_type ty in
        let init = Fir.{Initialiser.ty; value} in
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

    let counter_type (prim : Fir.Type.Prim.t) : Fir.Type.t =
      Fir.Type.(
        make
          (Basic.make prim ~is_atomic:false)
          ~is_pointer:false ~is_volatile:false)

    let gen (scope : Common.Scope.t) (prim_type : Fir.Type.Prim.t) :
        t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let+ lc_var = fresh_var scope in
        {var= lc_var; ty= counter_type prim_type})
  end

  module Simple = struct
    type t = {lc: Counter.t; up_to: Fir.Constant.t} [@@deriving sexp, fields]

    let src_exprs (x : t) : Fir.Expression.t list =
      [ Fir.Expression.constant x.up_to
      ; (* The loop counter is effectively being read from as well as written
           to each iteration. *)
        Counter.lc_expr x.lc ]

    module type S_action =
      Fuzz.Action_types.S with type Payload.t = t Fuzz.Payload_impl.Pathed.t
  end

  module Kv = struct
    type t =
      {lc: Counter.t; kv_val: Fir.Constant.t; kv_expr: Fir.Expression.t}
    [@@deriving sexp, fields]

    module type S_action =
      Fuzz.Action_types.S with type Payload.t = t Fuzz.Payload_impl.Pathed.t

    let src_exprs (x : t) : Fir.Expression.t list =
      [ x.kv_expr
      ; (* The loop counter is effectively being read from as well as written
           to each iteration. *)
        Counter.lc_expr x.lc ]

    let direction (i : Fir.Flow_block.For.Simple.Inclusivity.t)
        ({kv_val; _} : t) : Fir.Flow_block.For.Simple.Direction.t =
      (* Try to avoid overflows by only going up if we're negative. *)
      match Fir.Constant.as_int kv_val with
      | Ok x when x < 0 ->
          Up i
      | _ ->
          Down i

    let control (i : Fir.Flow_block.For.Simple.Inclusivity.t) (payload : t) :
        Act_fir.Flow_block.For.Simple.t =
      { lvalue=
          Accessor.construct Fir.Lvalue.variable
            (Common.Litmus_id.variable_name payload.lc.var)
      ; direction= direction i payload
      ; init_value= Fir.Expression.constant payload.kv_val
      ; cmp_value= payload.kv_expr }

    let gen_lc (scope : Common.Scope.t)
        (kv_var : Fir.Env.Record.t Common.C_named.t) :
        Counter.t Fuzz.Payload_gen.t =
      let prim =
        Fir.(
          kv_var.@(Common.C_named.value @> Env.Record.type_of
                   @> Type.Access.basic_type @> Type.Basic.Access.prim))
      in
      Counter.gen scope prim

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
           ~error:(Error.of_string "chosen value should have a known value")
      |> Fuzz.Payload_gen.Inner.return

    let known_value_var (scope : Common.Scope.t) :
        Fir.Env.Record.t Common.C_named.t Fuzz.Payload_gen.t =
      (* TODO(@MattWindsor91): this shares a lot of DNA with atomic_store
         redundant. *)
      Fuzz.Payload_gen.(
        let* vs = vars in
        let env =
          Fuzz.Var.Map.env_satisfying_all ~scope vs ~predicates:kv_var_preds
        in
        lift_quickcheck (Fir.Env.gen_random_var_with_record env))

    let gen (where : Fuzz.Path.With_meta.t) : t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let scope = Common.Scope.Local (Fuzz.Path.tid where.path) in
        let* kv_var = known_value_var scope in
        let* lc = gen_lc scope kv_var in
        let* kv_val = known_value_val kv_var in
        let+ kv_expr = access_of_var kv_var in
        {lc; kv_val; kv_expr})
  end
end

(* 'Payload' gets shadowed a lot later on. *)
open struct
  module Pd = Payload
end

module Insert = struct
  let prefix_name (x : Common.Id.t) : Common.Id.t =
    prefix_name Common.Id.("insert" @: "for" @: x)

  module Kv_never : Payload.Kv.S_action = struct
    (* TODO(@MattWindsor91): maybe, if we make the loop counter always store
       the KV, we can have that the loop counter persists that KV and can be
       reliable used as such. *)
    let name = prefix_name Common.Id.("kv-never" @: empty)

    let readme : string Lazy.t =
      lazy
        {| Introduces a loop that initialises its
      (fresh) counter to the known value of an existing variable and
      compares it in such a way as to be dead-code. |}

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(Pd.Counter.can_make + kv_available Insert)

    let path_filter = kv_live_path_filter

    module Payload = struct
      type t = Payload.Kv.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

      let gen =
        Fuzz.Payload_impl.Pathed.gen Insert path_filter Payload.Kv.gen
    end

    let recommendations (_ : Payload.t) : Common.Id.t list =
      (* No point introducing dead-code actions if we're introducing dead
         code. *)
      []

    let make_for (payload : Pd.Kv.t) : Fuzz.Subject.Statement.t =
      (* Comparing a var against its known value using < or >. *)
      let control = Pd.Kv.control Exclusive payload in
      let body = Fuzz.Subject.Block.make_dead_code () in
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.for_loop_simple ~control body))

    (* TODO(@MattWindsor91): unify this with things? *)
    let run (test : Fuzz.Subject.Test.t)
        ~(payload : Pd.Kv.t Fuzz.Payload_impl.Pathed.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Let_syntax.(
          let p = payload.payload in
          let%bind test' = Pd.Counter.declare_and_register p.lc test in
          let%bind () =
            add_expression_dependencies p.kv_expr
              ~scope:(Local (Fuzz.Path.tid payload.where.path))
          in
          Fuzz.Payload_impl.Pathed.insert payload
            ~filter:kv_live_path_filter_static ~test:test' ~f:make_for))
  end
end

module Surround = struct
  let prefix_name (x : Common.Id.t) : Common.Id.t =
    prefix_name Common.Id.("surround" @: "for" @: x)

  module Simple : Payload.Simple.S_action = Fuzz.Action.Make_surround (struct
    let name = prefix_name Common.Id.("simple" @: empty)

    let surround_with : string = "for-loops"

    let readme_suffix : string =
      {| The for loop initialises its (fresh) counter to zero, then counts
        upwards to a random, small, constant value.  This action does not
        surround non-generated or loop-unsafe statements. |}

    let path_filter (_ : Fuzz.State.t) =
      (* The restriction on not being in an execute-multiple block isn't for
         soundness, it's to prevent the fuzzer from deeply nesting multiple
         loops, which can quickly lead to overlong run times. *)
      Fuzz.Path_filter.(
        live_loop_surround
        + Fuzz.Path_filter.forbid_flag In_execute_multi
        + require_end_check (Stm_no_meta_restriction Once_only))

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        Pd.Counter.can_make
        + M.(
            lift_state path_filter
            >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      include Payload.Simple

      let gen (where : Fuzz.Path.With_meta.t) :
          Payload.Simple.t Fuzz.Payload_gen.t =
        let scope = Common.Scope.Local (Fuzz.Path.tid where.path) in
        Fuzz.Payload_gen.(
          let* lc_var = fresh_var scope in
          let lc : Pd.Counter.t = {var= lc_var; ty= Fir.Type.int ()} in
          let+ up_to =
            lift_quickcheck
              Base_quickcheck.Generator.small_strictly_positive_int
          in
          {Payload.Simple.lc; up_to= Int up_to})
    end

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      [Flow_dead.Insert.Early_out_loop_end.name]

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Pd.Counter.declare_and_register payload.lc test

    let control (payload : Payload.t) : Act_fir.Flow_block.For.Simple.t =
      { Fir.Flow_block.For.Simple.lvalue=
          Accessor.construct Fir.Lvalue.variable
            (Common.Litmus_id.variable_name payload.lc.var)
      ; direction= Up Inclusive
      ; init_value= Fir.Expression.int_lit 0
      ; cmp_value= Fir.Expression.constant payload.up_to }

    let wrap (statements : Fuzz.Subject.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Subject.Statement.t =
      let control = control payload in
      let body = Fuzz.Subject.Block.make_generated ~statements () in
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.for_loop_simple body ~control))
  end)

  module Kv_once : Payload.Kv.S_action = Fuzz.Action.Make_surround (struct
    let name = prefix_name Common.Id.("kv-once" @: empty)

    let surround_with : string = "for-loops"

    let readme_suffix : string =
      {| The for loop initialises its
      (fresh) counter to the known value of an existing variable and
      compares it in such a way as to execute only once. |}

    let path_filter = kv_live_path_filter

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(Pd.Counter.can_make + kv_available Transform_list)

    module Payload = Payload.Kv

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      [Flow_dead.Insert.Early_out_loop_end.name]

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Pd.Counter.declare_and_register payload.lc test

    let wrap (statements : Fuzz.Subject.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Subject.Statement.t =
      let control = Pd.Kv.control Inclusive payload in
      let body = Fuzz.Subject.Block.make_generated ~statements () in
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.for_loop_simple ~control body))
  end)

  module Dead :
    Fuzz.Action_types.S
      with type Payload.t = Fir.Flow_block.For.t Fuzz.Payload_impl.Pathed.t =
  Fuzz.Action.Make_surround (struct
    let name = prefix_name Common.Id.("dead" @: empty)

    let surround_with : string = "for-loops"

    let readme_suffix : string =
      {| This action introduces arbitrary, occasionally nonsensical for loops
         into dead code. |}

    let path_filter (_ : Fuzz.State.t) : Fuzz.Path_filter.t =
      (* We make sure to handle the case where we have no variables available
         to us in the generator, so the path filter doesn't have to. *)
      Fuzz.Path_filter.require_flag In_dead_code

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        Pd.Counter.can_make
        + M.(
            lift_state path_filter
            >>= is_filter_constructible ~kind:Transform_list))

    module Payload = struct
      type t = Fir.Flow_block.For.t [@@deriving sexp]

      let src_exprs x = x.@*(Fir.Flow_block.For.exprs)

      let gen_cmp (env : Fir.Env.t) : Fir.Expression.t Q.Generator.t =
        let module CInt = Fir_gen.Expr.Int_values (struct
          let env = env
        end) in
        let module CBool = Fir_gen.Expr.Bool_values (struct
          let env = env
        end) in
        Q.Generator.union
          [CInt.quickcheck_generator; CBool.quickcheck_generator]

      let gen_assign (env : Fir.Env.t) : Fir.Assign.t option Q.Generator.t =
        Option.value_map
          (Fir_gen.Assign.any ~src:env ~dst:env)
          ~default:(Q.Generator.return None) ~f:Q.Generator.option

      let gen' (env : Fir.Env.t) : t Q.Generator.t =
        Q.Generator.(
          Let_syntax.(
            let%map init = gen_assign env
            and cmp = option (gen_cmp env)
            and update = gen_assign env in
            Fir.Flow_block.For.make ?init ?cmp ?update ()))

      let gen (path : Fuzz.Path.With_meta.t) : t Fuzz.Payload_gen.t =
        Fuzz.(
          Payload_gen.(
            let* env = env_at_path path in
            lift_quickcheck (gen' env)))
    end

    let recommendations (_ : Payload.t Fuzz.Payload_impl.Pathed.t) :
        Common.Id.t list =
      (* No need to recommend any dead-code introduction actions; this action
         already introduces dead code! *)
      []

    let run_pre (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      (* No need to do housekeeping if we're in dead code. *)
      ignore payload ;
      Fuzz.State.Monad.return test

    let wrap (statements : Fuzz.Subject.Statement.t list)
        ~(payload : Payload.t) : Fuzz.Subject.Statement.t =
      let body = Fuzz.Subject.Block.make_dead_code ~statements () in
      let loop = Fir.Flow_block.for_loop body ~control:payload in
      Fir.(Accessor.construct Statement.flow loop)
  end)
end
