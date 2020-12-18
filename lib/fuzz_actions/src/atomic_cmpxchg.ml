(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in
   the project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("atomic" @: "cmpxchg" @: rest)

module Insert = struct
  module Inner_payload = struct
    type t =
      { out_var: Common.Litmus_id.t
      ; exp_var: Common.Litmus_id.t
      ; exp_val: Fir.Constant.t
      ; cmpxchg: Fir.Expression.t Fir.Atomic_cmpxchg.t }
    [@@deriving compare, sexp]

    let gen_obj (dst : Fir.Env.t) :
        (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      let module Dst = struct
        let env = dst
      end in
      let module Obj = Fir_gen.Address.Atomic_int_pointers (Dst) in
      Base_quickcheck.Generator.(
        filter_map Obj.quickcheck_generator ~f:(fun obj ->
            dst
            |> Fir.Env.known_value ~id:obj.@(Fir.Address.variable_of)
            |> Result.ok
            |> Option.bind ~f:(Option.map ~f:(fun v -> (obj, v)))))

    let gen_mos (allow_ub : bool) :
        (Fir.Mem_order.t * Fir.Mem_order.t) Base_quickcheck.Generator.t =
      let gfail = Fir.Mem_order.gen_cmpxchg_fail in
      Base_quickcheck.Generator.(
        Let_syntax.(
          let%bind succ = Fir.Mem_order.quickcheck_generator in
          let%map fail =
            if allow_ub then gfail
            else filter gfail ~f:(fun fail -> Fir.Mem_order.(fail <= succ))
          in
          (succ, fail)))

    let gen ~(src : Fir.Env.t) ~(dst : Fir.Env.t) ~(vars : Fuzz.Var.Map.t)
        ~(tid : int) ~(strength : Fir.Atomic_cmpxchg.Strength.t)
        ~(gen_obj :
              Fir.Env.t
           -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t)
        ~(allow_ub : bool) : t Base_quickcheck.Generator.t =
      let module Expr = Fir_gen.Expr.Int_values (struct
        let env = src
      end) in
      Base_quickcheck.Generator.(
        Let_syntax.(
          let%bind out_var_c, exp_var_c =
            vars
            |> Fuzz.Var.Map.gen_fresh_vars ~n:2
            >>| Fn.compose Or_error.ok_exn Tx.List.two
          in
          let%bind succ, fail = gen_mos allow_ub in
          let%bind obj, exp_val = gen_obj dst in
          let%map desired = Expr.quickcheck_generator in
          let expected =
            Accessor.construct Fir.Address.variable_ref exp_var_c
          in
          { out_var= Common.Litmus_id.local tid out_var_c
          ; exp_var= Common.Litmus_id.local tid exp_var_c
          ; exp_val
          ; cmpxchg=
              { Fir.Atomic_cmpxchg.obj
              ; expected
              ; desired
              ; strength
              ; succ
              ; fail } }))
  end

  let int_name (rest : Common.Id.t) : Common.Id.t =
    prefix_name Common.Id.("insert" @: "int" @: rest)

  module Int_succeed :
    Fuzz.Action_types.S
      with type Payload.t = Inner_payload.t Fuzz.Payload_impl.Pathed.t =
  Storelike.Make (struct
    let name = int_name Common.Id.("succeed" @: empty)

    let readme_preamble : string list =
      [ {| Inserts an atomic int compare-exchange that always succeeds, and a new
        local Boolean variable that receives its result.

        This compare-exchange guarantees static success by storing the
        known value of a variable to another fresh variable, then using that
        as the 'expected' value.

        The new variable is set up such that it has a known value of 'true'.
      |}
      ]

    type t = Inner_payload.t [@@deriving sexp]

    let recommendations (_ : t Fuzz.Payload_impl.Pathed.t) : Common.Id.t list
        =
      []

    let path_filter = Fuzz.Path_filter.zero

    let extra_dst_restrictions =
      [Fuzz.Var.Record.can_safely_modify; Fuzz.Var.Record.has_known_value]

    module Flags = struct
      let erase_known_values = true

      (* These compare-exchanges are always unsafe in a loop, since they
         change the value of the destination; future iterations of the same
         compare-exchange will fail, and not only return false but also
         change the value of the expected-variable. See issue 212. *)
      let execute_multi_safe = `Never
    end

    let gen :
           src:Fir.Env.t
        -> dst:Fir.Env.t
        -> vars:Fuzz.Var.Map.t
        -> tid:int
        -> Inner_payload.t Base_quickcheck.Generator.t =
      Inner_payload.gen ~allow_ub:false ~strength:Strong
        ~gen_obj:Inner_payload.gen_obj

    let dst_type : Fir.Type.Basic.t = Fir.Type.Basic.int ~is_atomic:true ()

    let new_local_cap : int = 2

    let new_locals (x : Inner_payload.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      (* TODO(@MattWindsor91): possibly piggyback off an existing known-true
         variable for out *)
      [ ( Common.Litmus_id.variable_name x.out_var
        , Fir.{Initialiser.ty= Fir.Type.bool (); value= Fir.Constant.truth}
        )
      ; ( Common.Litmus_id.variable_name x.exp_var
        , Fir.{Initialiser.ty= Fir.Type.int (); value= x.exp_val} ) ]

    let src_exprs (x : Inner_payload.t) : Fir.Expression.t list =
      [ x.cmpxchg.desired
      ; Fir.Expression.address x.cmpxchg.obj
      ; Fir.Expression.address x.cmpxchg.expected ]

    let dst_ids (x : Inner_payload.t) : Common.C_id.t list =
      (* exp_val/expected and out_val have known values, so we don't treat
         them as dests here. This might be a bad idea? *)
      x.cmpxchg.@*(Fir.Atomic_cmpxchg.obj @> Fir.Address.variable_of)

    let to_stms (x : Inner_payload.t) :
        meta:Fuzz.Metadata.t -> Fuzz.Subject.Statement.t list =
      (* We shouldn't need a specific assignment of the expected variable,
         since, as it gets a dependency put on it, it should keep the
         expected value from its initialiser. *)
      let cmpxchg_expr =
        Fir.Expression.atomic (Fir.Atomic_expression.cmpxchg x.cmpxchg)
      in
      let cmpxchg_assign =
        Fir.Assign.(
          Accessor.construct Fir.Lvalue.variable
            (Common.Litmus_id.variable_name x.out_var)
          @= cmpxchg_expr)
      in
      Storelike.lift_prims
        [Accessor.construct Fir.Prim_statement.assign cmpxchg_assign]
  end)
end
