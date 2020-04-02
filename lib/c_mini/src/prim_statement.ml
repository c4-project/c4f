(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
end

(* This module wrapper exists to let us refer to the generated variant
   constructors in Base_map. *)
module P = struct
  type t =
    | Assign of Assign.t
    | Atomic of Atomic_statement.t
    | Early_out of Early_out.t
    | Label of Ac.C_id.t
    | Goto of Ac.C_id.t
    | Nop
    | Procedure_call of Call.t
  [@@deriving variants, sexp, compare, equal]
end

include P

let atomic_cmpxchg (a : Atomic_cmpxchg.t) : t =
  atomic (Atomic_statement.atomic_cmpxchg a)

let atomic_fence (a : Atomic_fence.t) : t =
  atomic (Atomic_statement.atomic_fence a)

let atomic_store (a : Atomic_store.t) : t =
  atomic (Atomic_statement.atomic_store a)

let break : t = Early_out Break

let continue : t = Early_out Continue

let return : t = Early_out Return

let reduce (type result) (x : t) ~(assign : Assign.t -> result)
    ~(atomic : Atomic_statement.t -> result)
    ~(early_out : Early_out.t -> result) ~(label : Ac.C_id.t -> result)
    ~(goto : Ac.C_id.t -> result) ~(nop : unit -> result)
    ~(procedure_call : Call.t -> result) : result =
  Variants.map x ~assign:(Fn.const assign) ~atomic:(Fn.const atomic)
    ~early_out:(Fn.const early_out) ~label:(Fn.const label)
    ~goto:(Fn.const goto)
    ~nop:(fun _ -> nop ())
    ~procedure_call:(Fn.const procedure_call)

let as_atomic : t -> Atomic_statement.t option = function
  | Atomic a ->
      Some a
  | Assign _ | Label _ | Early_out _ | Goto _ | Nop | Procedure_call _ ->
      None

let is_atomic (p : t) : bool = Option.is_some (as_atomic p)

let as_early_out : t -> Early_out.t option = function
  | Early_out e ->
      Some e
  | Assign _ | Atomic _ | Label _ | Goto _ | Nop | Procedure_call _ ->
      None

let as_label : t -> Act_common.C_id.t option = function
  | Label l ->
      Some l
  | Assign _ | Atomic _ | Early_out _ | Goto _ | Nop | Procedure_call _ ->
      None

let is_label (p : t) : bool = Option.is_some (as_label p)

module Base_map (M : Monad.S) = struct
  let bmap (x : t) ~(assign : Assign.t -> Assign.t M.t)
      ~(atomic : Atomic_statement.t -> Atomic_statement.t M.t)
      ~(early_out : Early_out.t -> Early_out.t M.t)
      ~(label : Ac.C_id.t -> Ac.C_id.t M.t)
      ~(goto : Ac.C_id.t -> Ac.C_id.t M.t)
      ~(procedure_call : Call.t -> Call.t M.t) : t M.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~assign:(assign >> M.map ~f:P.assign)
        ~atomic:(atomic >> M.map ~f:P.atomic)
        ~early_out:(early_out >> M.map ~f:P.early_out)
        ~label:(label >> M.map ~f:P.label)
        ~goto:(goto >> M.map ~f:P.goto)
        ~nop:(fun () -> M.return P.nop)
        ~procedure_call:(procedure_call >> M.map ~f:P.procedure_call))
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

  module On_monad (M : Monad.S) = struct
    module SBase = Base_map (M)
    module AM = Basic.A.On_monad (M)
    module CM = Basic.C.On_monad (M)
    module TM = Basic.T.On_monad (M)

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
  module C = Call.On_lvalues
  module T = Atomic_statement.On_lvalues
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
