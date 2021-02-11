(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  | Constant
  | Address
  | Atomic of Atomic_class.t option
  | Bop of Op.Binary.t option
  | Uop of Op.Unary.t option
  | Ternary
[@@deriving compare, equal, sexp]

include Class.Make_ext (struct
  type nonrec t = t

  type 'm elt = Expression.t

  let class_matches (clazz : t) ~(template : t) : bool =
    match (clazz, template) with
    | Constant, Constant
     |Address, Address
     |Ternary, Ternary
     |Atomic _, Atomic None
     |Bop _, Bop None
     |Uop _, Uop None ->
        true
    | Atomic (Some clazz), Atomic (Some template) ->
        Atomic_class.matches clazz ~template
    | Bop (Some op), Bop (Some op') -> Op.Binary.equal op op'
    | Uop (Some op), Uop (Some op') -> Op.Unary.equal op op'
    | _ -> false

  let classify : Expression.t -> t option =
    Expression.reduce_step
      ~constant:(Fn.const (Some Constant))
      ~address:(Fn.const (Some Address)) ~ternary:(Fn.const (Some Ternary))
      ~atomic:(fun x -> Some (Atomic (Atomic_class.classify_expr x)))
      ~bop:(fun o _ _ -> Some (Bop (Some o)))
      ~uop:(fun o _ -> Some (Uop (Some o)))

  let classify_rec : Expression.t -> t list =
    Expression.reduce
      ~constant:(Fn.const [Constant])
      ~address:(Fn.const [Address])
      ~atomic:(fun x ->
        let inner = Atomic_expression.On_expressions.to_list x in
        List.concat ([Atomic (Atomic_class.classify_expr x)] :: inner) )
      ~bop:(fun o l r -> Bop (Some o) :: (l @ r))
      ~uop:(fun o x -> Uop (Some o) :: x)
      ~ternary:(fun {if_; then_; else_} -> Ternary :: (if_ @ then_ @ else_))
end)
