(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Ac = Act_common
module Identifier = Act_c_lang.Ast_basic.Identifier

module Bop = struct
  type t = Eq | L_and | L_or
  [@@deriving sexp, variants, compare, equal, quickcheck]
end

type t =
  | Constant of Constant.t
  | Lvalue of Lvalue.t
  | Atomic_load of Atomic_load.t
  | Bop of Bop.t * t * t
[@@deriving sexp, variants, compare, equal]

let bool_lit (b : bool) : t = constant (Constant.bool b)

let eq : t -> t -> t = bop Bop.Eq

let l_and : t -> t -> t = bop Bop.L_and

let l_or : t -> t -> t = bop Bop.L_or

let reduce (expr : t) ~(constant : Constant.t -> 'a)
    ~(lvalue : Lvalue.t -> 'a) ~(atomic_load : Atomic_load.t -> 'a)
    ~(bop : Bop.t -> 'a -> 'a -> 'a) : 'a =
  let rec mu = function
    | Constant k ->
        constant k
    | Lvalue l ->
        lvalue l
    | Atomic_load ld ->
        atomic_load ld
    | Bop (b, x, y) ->
        bop b (mu x) (mu y)
  in
  mu expr

let anonymise = function
  | Constant k ->
      `A k
  | Lvalue l ->
      `B l
  | Bop (b, x, y) ->
      `C (b, x, y)
  | Atomic_load ld ->
      `D ld

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    module A = Atomic_load.On_addresses.On_monad (M)

    let rec map_m x ~f =
      Variants.map x
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 M.return)
        ~bop:
          (F.proc_variant3 (fun (b, l, r) ->
               M.Let_syntax.(
                 let%bind l' = map_m l ~f in
                 let%map r' = map_m r ~f in
                 (b, l', r'))))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module A = Atomic_load.On_lvalues.On_monad (M)
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 f)
        ~bop:
          (F.proc_variant3 (fun (b, l, r) ->
               M.Let_syntax.(
                 let%bind l' = map_m l ~f in
                 let%map r' = map_m r ~f in
                 (b, l', r'))))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_identifiers :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Identifier.t =
  Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)

module Type_check (E : Env_types.S) = struct
  module Lv = Lvalue.Type_check (E)
  module Ld = Atomic_load.Type_check (E)

  let type_of_resolved_bop (b : Bop.t) (l_type : Type.t) (r_type : Type.t) :
      Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind u_type =
        if Type.equal l_type r_type then Or_error.return l_type
        else
          Or_error.error_s
            [%message
              "Operand types are not equal"
                ~operator:(b : Bop.t)
                ~left:(l_type : Type.t)
                ~right:(r_type : Type.t)]
      in
      match b with
      | Eq ->
          return Type.(bool ())
      | L_and | L_or ->
          if Type.equal Type.(bool ()) u_type then Or_error.return u_type
          else
            Or_error.error_s
              [%message
                "Operand types must be 'bool'"
                  ~operator:(b : Bop.t)
                  ~left:(l_type : Type.t)
                  ~right:(r_type : Type.t)])

  let rec type_of : t -> Type.t Or_error.t = function
    | Constant k ->
        Or_error.return (Constant.type_of k)
    | Lvalue l ->
        Lv.type_of l
    | Bop (b, l, r) ->
        type_of_bop b l r
    | Atomic_load ld ->
        Ld.type_of ld

  and type_of_bop (b : Bop.t) (l : t) (r : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind l_type = type_of l and r_type = type_of r in
      type_of_resolved_bop b l_type r_type)
end

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Quickcheck.Observer.(
    fixed_point (fun mu ->
        unmap ~f:anonymise
          [%quickcheck.observer:
            [ `A of Constant.t
            | `B of Lvalue.t
            | `C of Bop.t * [%custom mu] * [%custom mu]
            | `D of Atomic_load.t ]]))

module Eval = struct end
