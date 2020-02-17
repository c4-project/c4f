(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Identifier = Act_c_lang.Ast_basic.Identifier

module Uop = struct
  type t = L_not [@@deriving sexp, variants, compare, equal, quickcheck]
end

module Bop = struct
  type t = Eq | L_and | L_or
  [@@deriving sexp, variants, compare, equal, quickcheck]
end

type t =
  | Constant of Constant.t
  | Address of Address.t
  | Atomic_load of Atomic_load.t
  | Bop of Bop.t * t * t
  | Uop of Uop.t * t
[@@deriving sexp, variants, compare, equal]

let lvalue (l : Lvalue.t) : t = address (Address.lvalue l)

let variable (v : Ac.C_id.t) : t = lvalue (Lvalue.variable v)

let of_variable_str_exn (s : string) : t =
  lvalue (Lvalue.of_variable_str_exn s)

let int_lit (i : int) : t = constant (Constant.int i)

let bool_lit (b : bool) : t = constant (Constant.bool b)

let truth : t = bool_lit true

let falsehood : t = bool_lit false

let eq : t -> t -> t = bop Bop.Eq

let l_and : t -> t -> t = bop Bop.L_and

let l_or : t -> t -> t = bop Bop.L_or

let l_not : t -> t = uop Uop.L_not

module Infix = struct
  let ( == ) : t -> t -> t = eq

  let ( && ) : t -> t -> t = l_and

  let ( || ) : t -> t -> t = l_or

  let ( ! ) : t -> t = l_not
end

let reduce_step (expr : t) ~(constant : Constant.t -> 'a)
    ~(address : Address.t -> 'a) ~(atomic_load : Atomic_load.t -> 'a)
    ~(bop : Bop.t -> t -> t -> 'a) ~(uop : Uop.t -> t -> 'a) : 'a =
  match expr with
  | Constant k ->
      constant k
  | Address l ->
      address l
  | Atomic_load ld ->
      atomic_load ld
  | Bop (b, x, y) ->
      bop b x y
  | Uop (u, x) ->
      uop u x

let reduce (expr : t) ~(constant : Constant.t -> 'a)
    ~(address : Address.t -> 'a) ~(atomic_load : Atomic_load.t -> 'a)
    ~(bop : Bop.t -> 'a -> 'a -> 'a) ~(uop : Uop.t -> 'a -> 'a) : 'a =
  let rec mu (expr : t) =
    let bop b l r = bop b (mu l) (mu r) in
    let uop u x = uop u (mu x) in
    reduce_step expr ~constant ~address ~atomic_load ~bop ~uop
  in
  mu expr

let anonymise = function
  | Constant k ->
      `A k
  | Address l ->
      `B l
  | Bop (b, x, y) ->
      `C (b, x, y)
  | Atomic_load ld ->
      `D ld
  | Uop (u, x) ->
      `E (u, x)

(** Does the legwork of implementing a particular type of traversal over
    expressions. *)
module Make_traversal (Basic : sig
  module Elt : Equal.S

  module A :
    Travesty.Traversable_types.S0
      with type t := Address.t
       and module Elt = Elt

  module C :
    Travesty.Traversable_types.S0
      with type t := Constant.t
       and module Elt = Elt

  module L :
    Travesty.Traversable_types.S0
      with type t := Atomic_load.t
       and module Elt = Elt
end) =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Basic.Elt

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    module A = Basic.A.On_monad (M)
    module C = Basic.C.On_monad (M)
    module L = Basic.L.On_monad (M)

    let rec map_m x ~f =
      Variants.map x
        ~constant:(F.proc_variant1 (C.map_m ~f))
        ~address:(F.proc_variant1 (A.map_m ~f))
        ~uop:
          (F.proc_variant2 (fun (u, x) ->
               M.Let_syntax.(
                 let%map x' = map_m x ~f in
                 (u, x'))))
        ~bop:
          (F.proc_variant3 (fun (b, l, r) ->
               M.Let_syntax.(
                 let%bind l' = map_m l ~f in
                 let%map r' = map_m r ~f in
                 (b, l', r'))))
        ~atomic_load:(F.proc_variant1 (L.map_m ~f))
  end
end)

(* TODO(@MattWindsor91): travesty *)
module Ignore (T : T) (Elt : Equal.S) = Travesty.Traversable.Make0 (struct
  type t = T.t

  module Elt = Elt

  module On_monad (M : Monad.S) = struct
    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      ignore f ; M.return x
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Make_traversal (struct
  module Elt = Address
  module A =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
  module C = Ignore (Constant) (Address)
  module L = Atomic_load.On_addresses
end)

module On_constants :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t =
Make_traversal (struct
  module Elt = Constant
  module A = Ignore (Address) (Constant)
  module C =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
  module L = Ignore (Atomic_load) (Constant)
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Make_traversal (struct
  module Elt = Lvalue
  module A = Address.On_lvalues
  module C = Ignore (Constant) (Lvalue)
  module L = Atomic_load.On_lvalues
end)

module Type_check (E : Env_types.S) = struct
  module Ad = Address.Type_check (E)
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

  let type_of_resolved_uop (u : Uop.t) (x_type : Type.t) : Type.t Or_error.t
      =
    match u with
    | L_not ->
        if Type.equal Type.(bool ()) x_type then Or_error.return x_type
        else
          Or_error.error_s
            [%message
              "Operand type must be 'bool'"
                ~operator:(u : Uop.t)
                ~operand:(x_type : Type.t)]

  let rec type_of : t -> Type.t Or_error.t = function
    | Constant k ->
        Or_error.return (Constant.type_of k)
    | Address l ->
        Ad.type_of l
    | Bop (b, l, r) ->
        type_of_bop b l r
    | Uop (u, x) ->
        type_of_uop u x
    | Atomic_load ld ->
        Ld.type_of ld

  and type_of_bop (b : Bop.t) (l : t) (r : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind l_type = type_of l and r_type = type_of r in
      type_of_resolved_bop b l_type r_type)

  and type_of_uop (u : Uop.t) (x : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind x_type = type_of x in
      type_of_resolved_uop u x_type)
end

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Quickcheck.Observer.(
    fixed_point (fun mu ->
        unmap ~f:anonymise
          [%quickcheck.observer:
            [ `A of Constant.t
            | `B of Address.t
            | `C of Bop.t * [%custom mu] * [%custom mu]
            | `D of Atomic_load.t
            | `E of Uop.t * [%custom mu] ]]))

module Eval = struct
  let eval_logical (l : t) (r : t) ~(short_value : bool)
      ~(mu : t -> Constant.t Or_error.t) : Constant.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind l_const = mu l in
      let%bind l_value = Constant.as_bool l_const in
      if Bool.equal l_value short_value then Or_error.return l_const
      else mu r)

  let eval_land = eval_logical ~short_value:false

  let eval_lor = eval_logical ~short_value:true

  let eval_eq (l : t) (r : t) ~(mu : t -> Constant.t Or_error.t) =
    Or_error.Let_syntax.(
      let%bind l_const = mu l in
      let%bind r_const = mu r in
      if Comparable.lift [%equal: Type.t] ~f:Constant.type_of l_const r_const
      then
        Or_error.return
          (Constant.bool ([%equal: Constant.t] l_const r_const))
      else
        Or_error.error_s
          [%message
            "eq: types of constants are incompatible"
              ~left:(l_const : Constant.t)
              ~right:(r_const : Constant.t)])

  let eval_bop (mu : t -> Constant.t Or_error.t) :
      Bop.t -> t -> t -> Constant.t Or_error.t = function
    | L_and ->
        eval_land ~mu
    | L_or ->
        eval_lor ~mu
    | Eq ->
        eval_eq ~mu

  let eval_lnot (x : t) ~(mu : t -> Constant.t Or_error.t) :
      Constant.t Or_error.t =
    Or_error.(x |> mu >>= Constant.as_bool >>| not >>| Constant.bool)

  let eval_uop (mu : t -> Constant.t Or_error.t) :
      Uop.t -> t -> Constant.t Or_error.t = function
    | L_not ->
        eval_lnot ~mu

  let eval_atomic_load (atomic_load : Atomic_load.t)
      ~(env : Address.t -> Constant.t Or_error.t) : Constant.t Or_error.t =
    (* We don't specifically handle memory order here, since we assume that
       the known-values environment refers to things that are already fully
       propagated through memory. *)
    atomic_load |> Atomic_load.src |> Address.deref |> env

  let as_constant (expr : t) ~(env : Address.t -> Constant.t Or_error.t) :
      Constant.t Or_error.t =
    let rec mu : t -> Constant.t Or_error.t =
      (* We reduce in single steps to support short-circuiting evaluation. *)
      reduce_step ~constant:Or_error.return ~address:env
        ~atomic_load:(eval_atomic_load ~env)
        ~bop:(fun o -> eval_bop mu o)
        ~uop:(fun o -> eval_uop mu o)
    in
    mu expr

  let empty_env (_ : Address.t) : Constant.t Or_error.t =
    Or_error.error_string
      "tried to access an address in an empty environment"
end
