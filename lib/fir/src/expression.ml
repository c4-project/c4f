(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Acc = struct
  type t =
    | Constant of Constant.t
    | Address of Address.t
    | Atomic of t Atomic_expression.t
    | Bop of Op.Binary.t * t * t
    | Uop of Op.Unary.t * t
    | Ternary of t Expr_ternary.t
  [@@deriving sexp, accessors, compare, equal]
end

include Acc

let constant = Accessor.construct constant

let address = Accessor.construct address

let atomic = Accessor.construct atomic

let bop o l r = Accessor.construct bop (o, l, r)

let uop o x = Accessor.construct uop (o, x)

let ternary t = Accessor.construct ternary t

let reduce_step (expr : t) ~(constant : Constant.t -> 'a)
    ~(address : Address.t -> 'a) ~(atomic : t Atomic_expression.t -> 'a)
    ~(bop : Op.Binary.t -> t -> t -> 'a) ~(uop : Op.Unary.t -> t -> 'a)
    ~(ternary : t Expr_ternary.t -> 'a) : 'a =
  match expr with
  | Constant k ->
      constant k
  | Address l ->
      address l
  | Atomic ld ->
      atomic ld
  | Bop (b, x, y) ->
      bop b x y
  | Uop (u, x) ->
      uop u x
  | Ternary t ->
      ternary t

let anonymise =
  reduce_step
    ~constant:(fun k -> `A k)
    ~address:(fun l -> `B l)
    ~atomic:(fun a -> `C a)
    ~bop:(fun b x y -> `D (b, x, y))
    ~uop:(fun u x -> `E (u, x))
    ~ternary:(fun t -> `F t)

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Base_quickcheck.Observer.(
    fixed_point (fun mu ->
        unmap ~f:anonymise
          [%quickcheck.observer:
            [ `A of Constant.t
            | `B of Address.t
            | `C of [%custom Atomic_expression.quickcheck_observer mu]
            | `D of Op.Binary.t * [%custom mu] * [%custom mu]
            | `E of Op.Unary.t * [%custom mu]
            | `F of [%custom Expr_ternary.quickcheck_observer mu] ]]))

let lvalue (l : Lvalue.t) : t = Address (Lvalue l)

let variable (v : Common.C_id.t) : t = lvalue (Variable v)

let of_variable_str_exn (s : string) : t =
  lvalue (Lvalue.of_variable_str_exn s)

let int_lit (i : int) : t = constant (Constant.int i)

let bool_lit (b : bool) : t = constant (Constant.bool b)

let truth : t = bool_lit true

let falsehood : t = bool_lit false

let eq : t -> t -> t = bop Op.Binary.eq

let ne : t -> t -> t = bop Op.Binary.ne

let lt : t -> t -> t = bop Op.Binary.lt

let le : t -> t -> t = bop Op.Binary.le

let ge : t -> t -> t = bop Op.Binary.ge

let gt : t -> t -> t = bop Op.Binary.gt

let add : t -> t -> t = bop Op.Binary.add

let sub : t -> t -> t = bop Op.Binary.sub

let l_and : t -> t -> t = bop Op.Binary.l_and

let l_or : t -> t -> t = bop Op.Binary.l_or

let l_not : t -> t = uop Op.Unary.L_not

module Infix = struct
  let ( == ) : t -> t -> t = eq

  let ( != ) : t -> t -> t = ne

  let ( < ) : t -> t -> t = lt

  let ( <= ) : t -> t -> t = le

  let ( >= ) : t -> t -> t = ge

  let ( > ) : t -> t -> t = gt

  let ( + ) : t -> t -> t = add

  let ( - ) : t -> t -> t = sub

  let ( && ) : t -> t -> t = l_and

  let ( || ) : t -> t -> t = l_or

  let ( ! ) : t -> t = l_not
end

let reduce (expr : t) ~(constant : Constant.t -> 'a)
    ~(address : Address.t -> 'a) ~(atomic : 'a Atomic_expression.t -> 'a)
    ~(bop : Op.Binary.t -> 'a -> 'a -> 'a) ~(uop : Op.Unary.t -> 'a -> 'a)
    ~(ternary : 'a Expr_ternary.t -> 'a) : 'a =
  let rec mu (expr : t) =
    let bop b l r = bop b (mu l) (mu r) in
    let uop u x = uop u (mu x) in
    let atomic x = atomic (Atomic_expression.On_expressions.map ~f:mu x) in
    let ternary t = ternary (Accessor.map Expr_ternary.exprs ~f:mu t) in
    reduce_step expr ~constant ~address ~atomic ~bop ~uop ~ternary
  in
  mu expr

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : t) ~(constant : Constant.t -> Constant.t Ap.t)
      ~(address : Address.t -> Address.t Ap.t)
      ~(atomic : t Atomic_expression.t -> t Atomic_expression.t Ap.t)
      ~(bop : Op.Binary.t * t * t -> (Op.Binary.t * t * t) Ap.t)
      ~(uop : Op.Unary.t * t -> (Op.Unary.t * t) Ap.t)
      ~(ternary : t Expr_ternary.t -> t Expr_ternary.t Ap.t) : t Ap.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce_step x
        ~constant:(constant >> Ap.map ~f:(Accessor.construct Acc.constant))
        ~address:(address >> Ap.map ~f:(Accessor.construct Acc.address))
        ~atomic:(atomic >> Ap.map ~f:(Accessor.construct Acc.atomic))
        ~bop:(fun o l r ->
          Ap.map ~f:(Accessor.construct Acc.bop) (bop (o, l, r)))
        ~uop:(fun o x -> Ap.map ~f:(Accessor.construct Acc.uop) (uop (o, x)))
        ~ternary:(ternary >> Ap.map ~f:(Accessor.construct Acc.ternary)))
end

let atomic_cmpxchg (f : t Atomic_cmpxchg.t) : t =
  atomic (Atomic_expression.cmpxchg f)

let atomic_fetch (f : t Atomic_fetch.t) : t =
  atomic (Atomic_expression.fetch f)

let atomic_load (l : Atomic_load.t) : t = atomic (Atomic_expression.load l)

module Type_check (E : Env_types.S) = struct
  module Ad = Address.Type_check (E)
  module At = Atomic_expression.Type_check (E)
  module Te = Expr_ternary.Type_check (E)

  let require_unified_bop_type (b : Op.Binary.t) ~(want : Type.t)
      ~(got : Type.t) : Type.t Or_error.t =
    Or_error.tag_s (Type.check want got)
      ~tag:
        [%message
          "Checking unified type of binary operation"
            ~operator:(b : Op.Binary.t)
            ~should_be:(want : Type.t)]

  let type_of_resolved_bop (b : Op.Binary.t) (l_type : Type.t)
      (r_type : Type.t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind u_type =
        if Type.equal l_type r_type then Or_error.return l_type
        else
          Or_error.error_s
            [%message
              "Operand types are not equal"
                ~operator:(b : Op.Binary.t)
                ~left:(l_type : Type.t)
                ~right:(r_type : Type.t)]
      in
      match b with
      | Rel _ ->
          return Type.(bool ())
      | Arith _ | Bitwise _ ->
          require_unified_bop_type b ~want:Type.(int ()) ~got:u_type
      | Logical _ ->
          require_unified_bop_type b ~want:Type.(bool ()) ~got:u_type)

  let type_of_resolved_uop (u : Op.Unary.t) (x_type : Type.t) :
      Type.t Or_error.t =
    match u with
    | L_not ->
        if Type.equal Type.(bool ()) x_type then Or_error.return x_type
        else
          Or_error.error_s
            [%message
              "Operand type must be 'bool'"
                ~operator:(u : Op.Unary.t)
                ~operand:(x_type : Type.t)]

  let type_of_atomic (a : Type.t Or_error.t Atomic_expression.t) :
      Type.t Or_error.t =
    Or_error.(
      a |> Atomic_expression.On_expressions.With_errors.sequence_m
      >>= At.type_of)

  let type_of_ternary (t : Type.t Or_error.t Expr_ternary.t) :
      Type.t Or_error.t =
    Or_error.(
      Utils.Accessor.On_error.map ~f:Fn.id Expr_ternary.exprs t
      >>= Te.type_of)

  let type_of : t -> Type.t Or_error.t =
    reduce
      ~constant:(Fn.compose Or_error.return Constant.type_of)
      ~address:Ad.type_of
      ~bop:(fun b l r ->
        Or_error.Let_syntax.(
          let%bind l = l and r = r in
          type_of_resolved_bop b l r))
      ~uop:(fun b -> Or_error.bind ~f:(type_of_resolved_uop b))
      ~atomic:type_of_atomic ~ternary:type_of_ternary
end
