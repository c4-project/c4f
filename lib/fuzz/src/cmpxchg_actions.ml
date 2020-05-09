(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open struct
  module Ac = Act_common
  module Cm = Act_c_mini
end

module Inner_payload = struct
  type t =
    { out_var: Ac.Litmus_id.t
    ; exp_var: Ac.Litmus_id.t
    ; exp_val: Cm.Constant.t
    ; cmpxchg: Cm.Expression.t Cm.Atomic_cmpxchg.t }
  [@@deriving compare, sexp]
end

module Int_always_succeed :
  Action_types.S with type Payload.t = Inner_payload.t Payload.Insertion.t =
Storelike.Make (struct
  let name =
    Act_common.Id.("cmpxchg" @: "make" @: "int" @: "always-succeed" @: empty)

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

  let path_filter = Path_filter.empty

  let extra_dst_restrictions =
    [ Storelike.Dst_restriction.forbid_dependencies
    ; Var.Record.has_known_value ]

  module Flags = struct
    let erase_known_values = true

    let respect_src_dependencies = true
  end

  let gen_vars ~(vars : Var.Map.t) :
      (Ac.C_id.t * Ac.C_id.t) Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.(
      Let_syntax.(
        let%bind out_var = Var.Map.gen_fresh_var vars in
        let%map exp_var =
          filter (Var.Map.gen_fresh_var vars) ~f:(fun id ->
              not (Ac.C_id.equal id out_var))
        in
        (out_var, exp_var)))

  let gen_obj ~(dst : Cm.Env.t) :
      (Cm.Address.t * Cm.Constant.t) Base_quickcheck.Generator.t =
    let module Dst = struct
      let env = dst
    end in
    let module Obj = Cm.Address_gen.Atomic_int_pointers (Dst) in
    Base_quickcheck.Generator.(
      filter_map Obj.quickcheck_generator ~f:(fun obj ->
          match Cm.Env.known_value dst ~id:(Cm.Address.variable_of obj) with
          | Ok (Some v) ->
              Some (obj, v)
          | _ ->
              None))

  let gen_mos : (Cm.Mem_order.t * Cm.Mem_order.t) Base_quickcheck.Generator.t
      =
    Base_quickcheck.Generator.(
      Let_syntax.(
        let%bind succ = Cm.Mem_order.quickcheck_generator in
        let%map fail =
          filter Cm.Mem_order.gen_cmpxchg_fail ~f:(fun fail ->
              Cm.Mem_order.(fail <= succ))
        in
        (succ, fail)))

  let gen ~(src : Cm.Env.t) ~(dst : Cm.Env.t) ~(vars : Var.Map.t)
      ~(tid : int) : Inner_payload.t Base_quickcheck.Generator.t =
    let module Src = struct
      let env = src
    end in
    let module Expr = Cm.Expression_gen.Int_values (Src) in
    Base_quickcheck.Generator.(
      Let_syntax.(
        let%bind out_var_c, exp_var_c = gen_vars ~vars in
        let%bind succ, fail = gen_mos in
        let%bind obj, exp_val = gen_obj ~dst in
        let%map desired = Expr.quickcheck_generator in
        let out_var = Ac.Litmus_id.local tid out_var_c in
        let exp_var = Ac.Litmus_id.local tid exp_var_c in
        let expected = Cm.Address.of_variable_ref exp_var_c in
        let cmpxchg =
          Cm.Atomic_cmpxchg.make ~obj ~expected ~desired ~succ ~fail
        in
        Inner_payload.{out_var; exp_var; exp_val; cmpxchg}))

  let dst_type : Cm.Type.Basic.t = Cm.Type.Basic.int ~is_atomic:true ()

  let new_locals (x : Inner_payload.t) : Cm.Initialiser.t Ac.C_named.Alist.t
      =
    [ ( Ac.Litmus_id.variable_name x.out_var
      , Cm.Initialiser.make ~ty:(Cm.Type.bool ()) ~value:Cm.Constant.truth ()
      )
    ; ( Ac.Litmus_id.variable_name x.exp_var
      , Cm.Initialiser.make ~ty:(Cm.Type.int ()) ~value:x.exp_val () ) ]

  let src_exprs (x : Inner_payload.t) : Cm.Expression.t list =
    [ Cm.Atomic_cmpxchg.desired x.cmpxchg
    ; Cm.Expression.address (Cm.Atomic_cmpxchg.obj x.cmpxchg)
    ; Cm.Expression.address (Cm.Atomic_cmpxchg.expected x.cmpxchg) ]

  let dst_ids (x : Inner_payload.t) : Ac.C_id.t list =
    (* exp_val/expected and out_val have known values, so we don't treat them
       as dests here. This might be a bad idea? *)
    [Cm.Address.variable_of (Cm.Atomic_cmpxchg.obj x.cmpxchg)]

  let to_stms (x : Inner_payload.t) : Cm.Prim_statement.t list =
    (* We shouldn't need a specific assignment of the expected variable,
       since, as it gets a dependency put on it, it should keep the expected
       value from its initialiser. *)
    let cmpxchg_expr =
      Cm.Expression.atomic (Cm.Atomic_expression.cmpxchg x.cmpxchg)
    in
    let cmpxchg_assign =
      Cm.Assign.make
        ~lvalue:(Cm.Lvalue.variable (Ac.Litmus_id.variable_name x.out_var))
        ~rvalue:cmpxchg_expr
    in
    [Cm.Prim_statement.assign cmpxchg_assign]
end)
