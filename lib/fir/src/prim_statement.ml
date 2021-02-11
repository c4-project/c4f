(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* This module wrapper exists to let us refer to the generated variant
   constructors in Base_map. *)
module P = struct
  type t =
    | Assign of Assign.t
    | Atomic of Atomic_statement.t
    | Early_out of Early_out.t
    | Label of Common.C_id.t
    | Goto of Common.C_id.t
    | Nop
    | Procedure_call of Call.t
  [@@deriving accessors, sexp, compare, equal]
end

include P

let break : t = Early_out Break

let continue : t = Early_out Continue

let return : t = Early_out Return

let value_map (type result) (x : t) ~(assign : Assign.t -> result)
    ~(atomic : Atomic_statement.t -> result)
    ~(early_out : Early_out.t -> result) ~(label : Common.C_id.t -> result)
    ~(goto : Common.C_id.t -> result) ~(nop : unit -> result)
    ~(procedure_call : Call.t -> result) : result =
  match x with
  | Assign a -> assign a
  | Atomic a -> atomic a
  | Early_out e -> early_out e
  | Label l -> label l
  | Goto g -> goto g
  | Nop -> nop ()
  | Procedure_call p -> procedure_call p

module Base_map (M : Applicative.S) = struct
  let bmap (x : t) ~(assign : Assign.t -> Assign.t M.t)
      ~(atomic : Atomic_statement.t -> Atomic_statement.t M.t)
      ~(early_out : Early_out.t -> Early_out.t M.t)
      ~(label : Common.C_id.t -> Common.C_id.t M.t)
      ~(goto : Common.C_id.t -> Common.C_id.t M.t)
      ~(procedure_call : Call.t -> Call.t M.t) : t M.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      value_map x
        ~assign:(assign >> M.map ~f:(fun a -> Assign a))
        ~atomic:(atomic >> M.map ~f:(fun a -> Atomic a))
        ~early_out:(early_out >> M.map ~f:(fun e -> Early_out e))
        ~label:(label >> M.map ~f:(fun l -> Label l))
        ~goto:(goto >> M.map ~f:(fun g -> Goto g))
        ~nop:(fun () -> M.return Nop)
        ~procedure_call:
          (procedure_call >> M.map ~f:(fun p -> Procedure_call p)))
end

(** Does the legwork of implementing a particular type of traversal over
    statements. *)
module Make_traversal (Basic : sig
  module Elt : Equal.S

  module A :
    Travesty.Traversable_types.S0
      with type t := Assign.t
       and module Elt = Elt

  module C :
    Travesty.Traversable_types.S0 with type t := Call.t and module Elt = Elt

  module T :
    Travesty.Traversable_types.S0
      with type t := Atomic_statement.t
       and module Elt = Elt
end) =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Basic.Elt

  module On (M : Applicative.S) = struct
    module SBase = Base_map (M)
    module AM = Basic.A.On (M)
    module CM = Basic.C.On (M)
    module TM = Basic.T.On (M)

    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      SBase.bmap x ~assign:(AM.map_m ~f) ~atomic:(TM.map_m ~f)
        ~early_out:M.return ~label:M.return ~goto:M.return
        ~procedure_call:(CM.map_m ~f)
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Make_traversal (struct
  module Elt = Lvalue
  module A = Assign.On_lvalues
  module C =
    Travesty.Traversable.Chain0 (Call.On_addresses) (Address.On_lvalues)
  module T =
    Travesty.Traversable.Chain0
      (Atomic_statement.On_addresses)
      (Address.On_lvalues)
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Make_traversal (struct
  module Elt = Address
  module A = Assign.On_addresses
  module C = Call.On_addresses
  module T = Atomic_statement.On_addresses
end)

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Make_traversal (struct
  module Elt = Expression
  module A = Assign.On_expressions
  module C = Call.On_expressions
  module T = Atomic_statement.On_expressions
end)

module On_atomics :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Atomic_statement.t = Make_traversal (struct
  module Elt = Atomic_statement
  module A = Travesty.Traversable.Const (Assign) (Atomic_statement)
  module C = Travesty.Traversable.Const (Call) (Atomic_statement)
  module T =
    Travesty.Traversable.Fix_elt
      (Travesty_containers.Singleton)
      (Atomic_statement)
end)
