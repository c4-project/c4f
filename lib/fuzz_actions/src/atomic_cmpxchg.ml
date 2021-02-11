(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("atomic" @: "cmpxchg" @: rest)

module Insert = struct
  module Inner_payload = struct
    type t =
      { out_var: Common.C_id.t
      ; exp_var: Common.C_id.t
      ; exp_val: Fir.Constant.t
            (* TODO(@MattWindsor91): this should be an expression, but that
               complicates initialisation. *)
      ; cmpxchg: Fir.Expression.t Fir.Atomic_cmpxchg.t }
    [@@deriving compare, sexp]

    let gen_obj_and_kv (dst : Fir.Env.t) :
        (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      let module Obj = Fir_gen.Address.Atomic_int_pointers (struct
        let env = dst
      end) in
      Base_quickcheck.Generator.(
        filter_map Obj.quickcheck_generator ~f:(fun obj ->
            dst
            |> Fir.Env.known_value ~id:obj.@(Fir.Address.variable_of)
            |> Result.ok
            |> Option.bind ~f:(Option.map ~f:(fun v -> (obj, v))) ))

    let gen_const :
        Fir.Type.Prim.t -> Fir.Constant.t Base_quickcheck.Generator.t =
      (* TODO(@MattWindsor91): can this be generalised? *)
      function
      | Int -> Fir.Constant.gen_int32
      | Bool -> Fir.Constant.gen_bool

    let gen_obj_and_arb (ty : Fir.Type.Prim.t) (dst : Fir.Env.t) :
        (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      let module Obj = Fir_gen.Address.Atomic_int_pointers (struct
        let env = dst
      end) in
      Base_quickcheck.Generator.(
        map2
          ~f:(fun obj exp -> (obj, exp))
          Obj.quickcheck_generator (gen_const ty))

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

    let gen_expr (ty : Fir.Type.Prim.t) :
        Fir.Env.t -> Fir.Expression.t Base_quickcheck.Generator.t =
      (* TODO(@MattWindsor91): can this be generalised? *)
      match ty with Int -> Fir_gen.Expr.int | Bool -> Fir_gen.Expr.bool

    let gen ?(strength : Fir.Atomic_cmpxchg.Strength.t option)
        ~(prim_type : Fir.Type.Prim.t) ~(src : Fir.Env.t) ~(dst : Fir.Env.t)
        ~(vars : Fuzz.Var.Map.t) ~tid:(_ : int)
        ~(gen_obj_and_exp :
              Fir.Env.t
           -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t )
        ~(allow_ub : bool) : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.(
        Let_syntax.(
          let%bind out_var, exp_var =
            vars
            |> Fuzz.Var.Map.gen_fresh_vars ~n:2
            >>| Fn.compose Or_error.ok_exn Tx.List.two
          in
          let%bind succ, fail = gen_mos allow_ub in
          let%bind obj, exp_val = gen_obj_and_exp dst in
          let%bind strength =
            Option.value_map strength ~f:return
              ~default:Fir.Atomic_cmpxchg.Strength.quickcheck_generator
          in
          let%map desired = gen_expr prim_type src in
          let expected =
            Accessor.construct Fir.Address.variable_ref exp_var
          in
          { out_var
          ; exp_var
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

  module type S =
    Fuzz.Action_types.S
      with type Payload.t = Inner_payload.t Fuzz.Payload_impl.Pathed.t

  module Make_int (Basic : sig
    val name : Common.Id.t
    (** [name] is the name suffix of the action. *)

    val readme_tail : string
    (** [readme_tail] is the tail of the readme after the boilerplate about
        doing a compare-exchange. *)

    val strength : Fir.Atomic_cmpxchg.Strength.t option
    (** [strength] is the target compare-exchange strength, if any.

        If not given, we randomly generate it.

        Ideally, we might take this through a flag and change the semantics
        of the action that way, but the storelike boilerplate doesn't support
        flags yet. *)

    val prim_type : Fir.Type.Prim.t
    (** [prim_type] is the primitive type of the compare-exchange. It must
        agree with the type of the object and expected variables, and is used
        to generate the desired value. *)

    val gen_obj_and_exp :
         Fir.Env.t
      -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t
    (** [gen_obj_and_exp dst] should generate both the target object address
        (from environment [dst]) and the expected value; the two are
        generated together in case the latter depends on the former. *)

    val can_succeed : bool
    (** [can_succeed] tracks whether the compare-exchange can succeed; if so,
        then the object value is potentially invalidated. *)

    val can_fail : bool
    (** [can_fail] tracks whether the compare-exchange can fail; if so, then
        the expected value is potentially invalidated. *)

    val needs_kv : bool
    (** [needs_kv] tracks whether the compare-exchange depends on a known
        object value. *)
  end) : S = Storelike.Make (struct
    let name = int_name Basic.name

    let readme_preamble : string list =
      [ Printf.sprintf
          {| Inserts a %s atomic int compare-exchange, and a new local Boolean
          variable that receives its result. |}
          (Option.value_map ~default:"strong or weak"
             ~f:Fir.Atomic_cmpxchg.Strength.to_string Basic.strength )
      ; Basic.readme_tail ]

    type t = Inner_payload.t [@@deriving sexp]

    let recommendations (_ : t Fuzz.Payload_impl.Pathed.t) : Common.Id.t list
        =
      []

    let path_filter = Fuzz.Path_filter.zero

    let extra_dst_restrictions =
      (* TODO(@MattWindsor91): make this a function of whether we're in
         deadcode or not. *)
      List.filter_opt
        [ Option.some_if Basic.can_succeed Fuzz.Var.Record.can_safely_modify
        ; Option.some_if Basic.needs_kv Fuzz.Var.Record.has_known_value ]

    module Flags = struct
      let erase_known_values = true

      (* Compare-exchanges with statically expected success/failure are
         always unsafe in non-dead loops, since they change the value of the
         destination; future iterations of the same compare-exchange will
         fail, and not only return false but also change the value of the
         expected-variable. See issue 212.

         Arbitrary compare-exchanges are still unsafe if there are dependency
         cycles: we don't care about them switching from failure to success
         or vice versa, but they can still clobber their dependencies. Since
         we encode read-modify-writes in such a way that c4f always computes
         dependency cycles over them, we have to set this to `Never. *)
      let execute_multi_safe = `Never
    end

    let gen :
           src:Fir.Env.t
        -> dst:Fir.Env.t
        -> vars:Fuzz.Var.Map.t
        -> tid:int
        -> Inner_payload.t Base_quickcheck.Generator.t =
      Inner_payload.gen ?strength:Basic.strength
        ~allow_ub:false (* for now *) ~gen_obj_and_exp:Basic.gen_obj_and_exp
        ~prim_type:Basic.prim_type

    let dst_type = Fir.Type.Basic.make ~is_atomic:true Basic.prim_type

    let new_local_cap : int = 2

    let new_locals (x : Inner_payload.t) :
        Fir.Initialiser.t Common.C_named.Alist.t =
      (* If we can either succeed or fail, it doesn't matter what this gets
         set to; otherwise, it needs to reflect the expected outcome to set
         up the preservation of the known value. *)
      let out_val = Basic.can_succeed in
      (* TODO(@MattWindsor91): possibly piggyback off an existing variable
         for out *)
      [ ( x.out_var
        , Fir.
            { Initialiser.ty= Fir.Type.bool ()
            ; value= Fir.Constant.bool out_val } )
      ; ( x.exp_var
        , Fir.
            { Initialiser.ty=
                Fir.Type.make (Fir.Type.Basic.strip_atomic dst_type)
            ; value= x.exp_val } ) ]

    let src_exprs (x : Inner_payload.t) : Fir.Expression.t list =
      [ x.cmpxchg.desired
      ; Fir.Expression.address x.cmpxchg.obj
      ; Fir.Expression.address x.cmpxchg.expected ]

    let dst_ids (x : Inner_payload.t) : Common.C_id.t list =
      List.filter_opt
        [ (* If the cmpxchg can succeed, the object can be clobbered with the
             desired value. *)
          Option.some_if Basic.can_succeed
            x.cmpxchg.@(Fir.Atomic_cmpxchg.obj @> Fir.Address.variable_of)
        ; (* If the cmpxchg can fail, the expected variable can be clobbered
             with the object value. *)
          Option.some_if Basic.can_fail
            x.cmpxchg.@(Fir.Atomic_cmpxchg.expected
                        @> Fir.Address.variable_of)
        ; (* If both possibilities are available, we can't guarantee a known
             value for the output variable. *)
          Option.some_if (Basic.can_succeed && Basic.can_fail) x.out_var ]

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
          Accessor.construct Fir.Lvalue.variable x.out_var @= cmpxchg_expr)
      in
      Storelike.lift_prims
        [Accessor.construct Fir.Prim_statement.assign cmpxchg_assign]
  end)

  module Int_succeed : S = Make_int (struct
    let name = Common.Id.("succeed" @: empty)

    let prim_type : Fir.Type.Prim.t = Int

    let readme_tail : string =
      {| This compare-exchange guarantees static success by storing the
        known value of a variable to another fresh variable, then using that
        as the 'expected' value.  This variable will have a known value of
        'true'. |}

    let gen_obj_and_exp :
           Fir.Env.t
        -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      Inner_payload.gen_obj_and_kv

    let needs_kv : bool = true

    let can_succeed : bool = true

    let can_fail : bool = false

    (* Succeeding cmpxchgs can't be weak, as weak cmpxchgs can spuriously
       fail. *)
    let strength : Fir.Atomic_cmpxchg.Strength.t option = Some Strong
  end)

  module Int_fail : S = Make_int (struct
    let name = Common.Id.("fail" @: empty)

    let prim_type : Fir.Type.Prim.t = Int

    let readme_tail : string =
      {| This compare-exchange guarantees static failure by storing the
        known value of a variable to another fresh variable, calculating a
        different value, then using that
        as the 'expected' value.  This variable will have a known value of
        'false'. |}

    let perturb_constant : Fir.Constant.t -> Fir.Constant.t =
      (* Trying to avoid overflow by heading towards 0. *)
      (* TODO(@MattWindsor91): can this be used elsewhere? *)
      function
      | Int k when k <= 0 -> Int (k + 1)
      | Int k -> Int (k - 1)
      | Bool b -> Bool (not b)

    let gen_obj_and_exp :
           Fir.Env.t
        -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      Fn.compose
        (Base_quickcheck.Generator.map ~f:(fun (o, e) ->
             (o, perturb_constant e) ) )
        Inner_payload.gen_obj_and_kv

    let needs_kv : bool = true

    let can_succeed : bool = false

    let can_fail : bool = true

    (* A failure is a failure, no matter how strong. *)
    let strength : Fir.Atomic_cmpxchg.Strength.t option = None
  end)

  module Int_arbitrary : S = Make_int (struct
    let name = Common.Id.("arbitrary" @: empty)

    let prim_type : Fir.Type.Prim.t = Int

    let readme_tail : string =
      {| This compare-exchange is entirely random. |}

    let gen_obj_and_exp :
           Fir.Env.t
        -> (Fir.Address.t * Fir.Constant.t) Base_quickcheck.Generator.t =
      Inner_payload.gen_obj_and_arb Int

    let needs_kv : bool = false

    let can_succeed : bool = true

    let can_fail : bool = true

    (* Arbitrary cmpxchgs can have any strength. *)
    let strength : Fir.Atomic_cmpxchg.Strength.t option = None
  end)
end
