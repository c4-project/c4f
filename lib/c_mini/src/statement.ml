(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

(** [P_if_statement] contains a fully type-parametrised if-statement type. *)
module P_if_statement = struct
  type ('meta, 'stm) t =
    { cond: Expression.t
    ; t_branch: ('meta, 'stm) Block.t
    ; f_branch: ('meta, 'stm) Block.t }
  [@@deriving sexp, fields, equal]
end

(** [P_while_loop] contains a fully type-parametrised while_loop type. *)
module P_while_loop = struct
  type ('meta, 'stm) t =
    { cond: Expression.t
    ; body: ('meta, 'stm) Block.t
    ; kind: [`While | `Do_while] }
  [@@deriving sexp, fields, equal]
end

(* We can't put this into its own P module because OCaml's type system won't
   let us declare systems of recursive types where each type is an
   abbreviation. However, declaring statement inline _also_ means we can't
   use ppx. Hopefully I picked the right one out of Scylla and Charybdis. *)
type 'meta statement =
  | Assign of Assign.t
  | Atomic_store of Atomic_store.t
  | Atomic_cmpxchg of Atomic_cmpxchg.t
  | Nop
  | If_stm of 'meta if_statement
  | While_loop of 'meta while_loop
[@@deriving sexp, equal]

and 'meta if_statement = ('meta, 'meta statement) P_if_statement.t

and 'meta while_loop = ('meta, 'meta statement) P_while_loop.t

type 'meta block = ('meta, 'meta statement) Block.t

module Ifs_base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)
  module O = Tx.Option.On_monad (M)

  let bmap (type m1 m2) (if_stm : m1 if_statement)
      ~(cond : Expression.t F.traversal)
      ~(t_branch : m1 block -> m2 block M.t)
      ~(f_branch : m1 block -> m2 block M.t) : m2 if_statement M.t =
    M.Let_syntax.(
      let%map cond' = cond if_stm.cond
      and t_branch' = t_branch if_stm.t_branch
      and f_branch' = f_branch if_stm.f_branch in
      P_if_statement.{cond= cond'; t_branch= t_branch'; f_branch= f_branch'})
end

module Whiles_base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)
  module O = Tx.Option.On_monad (M)

  let bmap (type m1 m2) (loop : m1 while_loop)
      ~(cond : Expression.t F.traversal) ~(body : m1 block -> m2 block M.t) :
      m2 while_loop M.t =
    M.Let_syntax.(
      let%map cond' = cond loop.cond and body' = body loop.body in
      P_while_loop.{cond= cond'; body= body'; kind= loop.kind})
end

module Main :
  Types.S_statement
    with type address := Address.t
     and type 'meta t = 'meta statement
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type lvalue := Lvalue.t
     and type 'meta assign := Assign.t
     and type 'meta atomic_cmpxchg := Atomic_cmpxchg.t
     and type 'meta atomic_store := Atomic_store.t
     and type 'meta if_stm := 'meta if_statement
     and type 'meta while_loop := 'meta while_loop = struct
  type 'meta t = 'meta statement [@@deriving sexp, equal]

  module Accessors = struct
    let assign x = Assign x

    let atomic_cmpxchg x = Atomic_cmpxchg x

    let atomic_store x = Atomic_store x

    let if_stm x = If_stm x

    let while_loop x = While_loop x

    let nop () = Nop
  end

  include Accessors

  let reduce (type meta result) (x : meta t) ~assign ~atomic_cmpxchg
      ~atomic_store ~if_stm ~while_loop ~nop : result =
    match x with
    | Assign x ->
        assign x
    | Atomic_cmpxchg x ->
        atomic_cmpxchg x
    | Atomic_store x ->
        atomic_store x
    | If_stm x ->
        if_stm x
    | While_loop x ->
        while_loop x
    | Nop ->
        nop ()

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (type m1 m2) (x : m1 t) ~assign ~atomic_cmpxchg ~atomic_store
        ~if_stm ~while_loop ~nop : m2 t M.t =
      Travesty_base_exts.Fn.Compose_syntax.(
        reduce x
          ~assign:(assign >> M.map ~f:Accessors.assign)
          ~atomic_cmpxchg:
            (atomic_cmpxchg >> M.map ~f:Accessors.atomic_cmpxchg)
          ~atomic_store:(atomic_store >> M.map ~f:Accessors.atomic_store)
          ~if_stm:(if_stm >> M.map ~f:Accessors.if_stm)
          ~while_loop:(while_loop >> M.map ~f:Accessors.while_loop)
          ~nop:(nop >> M.map ~f:(Fn.const Nop)))
  end

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module WB = Whiles_base_map (M)
      module Bk = Block.On_monad (M)

      (* We have to unroll the map over if/while here, because otherwise we
         end up with unsafe module recursion. *)

      let rec map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x ~assign:M.return ~atomic_cmpxchg:M.return
          ~atomic_store:M.return
          ~if_stm:
            (IB.bmap ~cond:M.return
               ~t_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f))
               ~f_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f)))
          ~while_loop:
            (WB.bmap ~cond:M.return
               ~body:(Bk.bi_map_m ~left:f ~right:(map_m ~f)))
          ~nop:M.return
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    type nonrec t = Meta.t t

    module Block_stms = Block.On_statements (Meta)

    (** Does the legwork of implementing a particular type of traversal over
        statements. *)
    module Make_traversal (Basic : sig
      module Elt : Equal.S

      module A :
        Travesty.Traversable_types.S0
          with type t := Assign.t
           and module Elt = Elt

      module C :
        Travesty.Traversable_types.S0
          with type t := Atomic_cmpxchg.t
           and module Elt = Elt

      module E :
        Travesty.Traversable_types.S0
          with type t := Expression.t
           and module Elt = Elt

      module S :
        Travesty.Traversable_types.S0
          with type t := Atomic_store.t
           and module Elt = Elt
    end) =
    Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Basic.Elt

      module On_monad (M : Monad.S) = struct
        module SBase = Base_map (M)
        module IBase = Ifs_base_map (M)
        module WBase = Whiles_base_map (M)
        module Bk = Block_stms.On_monad (M)
        module AM = Basic.A.On_monad (M)
        module CM = Basic.C.On_monad (M)
        module EM = Basic.E.On_monad (M)
        module SM = Basic.S.On_monad (M)

        (* We also have to unroll the map over if/while here. *)

        let rec map_m x ~f =
          SBase.bmap x ~assign:(AM.map_m ~f) ~atomic_store:(SM.map_m ~f)
            ~atomic_cmpxchg:(CM.map_m ~f) ~if_stm:(map_m_ifs ~f)
            ~while_loop:(map_m_whiles ~f) ~nop:M.return

        and map_m_ifs x ~f =
          IBase.bmap x ~cond:(EM.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(map_m ~f))
            ~f_branch:(Bk.map_m ~f:(map_m ~f))

        and map_m_whiles x ~f =
          WBase.bmap x ~cond:(EM.map_m ~f) ~body:(Bk.map_m ~f:(map_m ~f))
      end
    end)

    module On_lvalues :
      Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
    Make_traversal (struct
      module Elt = Lvalue
      module A = Assign.On_lvalues
      module C = Atomic_cmpxchg.On_lvalues
      module E = Expression.On_lvalues
      module S = Atomic_store.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module A = Assign.On_addresses
      module C = Atomic_cmpxchg.On_addresses
      module E = Expression.On_addresses
      module S = Atomic_store.On_addresses
    end)

    module On_identifiers :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Act_common.C_id.t =
      Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)
  end
end

include Main

module If :
  Types.S_if_statement
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta if_statement = struct
  type 'meta t = 'meta if_statement [@@deriving sexp, equal]

  let cond = P_if_statement.cond

  let f_branch = P_if_statement.f_branch

  let t_branch = P_if_statement.t_branch

  let make = P_if_statement.Fields.create

  module Base_map (M : Monad.S) = Ifs_base_map (M)

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module Mn = On_meta.On_monad (M)
      module Bk = Block.On_monad (M)

      (* We have to unroll the map over if statements here, because otherwise
         we end up with unsafe module recursion. *)

      let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x ~cond:M.return
          ~t_branch:(Bk.bi_map_m ~left:f ~right:(Mn.map_m ~f))
          ~f_branch:(Bk.bi_map_m ~left:f ~right:(Mn.map_m ~f))
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    type nonrec t = Meta.t t

    module Block_stms = Block.On_statements (Meta)
    module Sm = With_meta (Meta)

    (** Does the legwork of implementing a particular type of traversal over
        if statements. *)
    module Make_traversal (Basic : sig
      module Elt : Equal.S

      module E :
        Travesty.Traversable_types.S0
          with type t := Expression.t
           and module Elt = Elt

      module S :
        Travesty.Traversable_types.S0
          with type t := Sm.t
           and module Elt = Elt
    end) =
    Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Basic.Elt

      module On_monad (M : Monad.S) = struct
        module IBase = Base_map (M)
        module Bk = Block_stms.On_monad (M)
        module EM = Basic.E.On_monad (M)
        module SM = Basic.S.On_monad (M)

        let map_m x ~f =
          IBase.bmap x ~cond:(EM.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(SM.map_m ~f))
            ~f_branch:(Bk.map_m ~f:(SM.map_m ~f))
      end
    end)

    module On_lvalues :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Lvalue.t = Make_traversal (struct
      module Elt = Lvalue
      module E = Expression.On_lvalues
      module S = Sm.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression.On_addresses
      module S = Sm.On_addresses
    end)

    module On_identifiers :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Act_common.C_id.t =
      Travesty.Traversable.Chain0
        (struct
          type nonrec t = t

          include On_lvalues
        end)
        (Lvalue.On_identifiers)
  end
end

module While :
  Types.S_while_loop
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta while_loop = struct
  type 'meta t = 'meta while_loop [@@deriving sexp, equal]

  let cond = P_while_loop.cond

  let body = P_while_loop.body

  let kind = P_while_loop.kind

  let make = P_while_loop.Fields.create

  module Base_map (M : Monad.S) = Whiles_base_map (M)

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module Mn = On_meta.On_monad (M)
      module Bk = Block.On_monad (M)

      let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x ~cond:M.return
          ~body:(Bk.bi_map_m ~left:f ~right:(Mn.map_m ~f))
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    type nonrec t = Meta.t t

    module Block_stms = Block.On_statements (Meta)
    module Sm = With_meta (Meta)

    (** Does the legwork of implementing a particular type of traversal over
        while loops. *)
    module Make_traversal (Basic : sig
      module Elt : Equal.S

      module E :
        Travesty.Traversable_types.S0
          with type t := Expression.t
           and module Elt = Elt

      module S :
        Travesty.Traversable_types.S0
          with type t := Sm.t
           and module Elt = Elt
    end) =
    Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Basic.Elt

      module On_monad (M : Monad.S) = struct
        module WBase = Base_map (M)
        module Bk = Block_stms.On_monad (M)
        module EM = Basic.E.On_monad (M)
        module SM = Basic.S.On_monad (M)

        let map_m x ~f =
          WBase.bmap x ~cond:(EM.map_m ~f) ~body:(Bk.map_m ~f:(SM.map_m ~f))
      end
    end)

    module On_lvalues :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Lvalue.t = Make_traversal (struct
      module Elt = Lvalue
      module E = Expression.On_lvalues
      module S = Sm.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression.On_addresses
      module S = Sm.On_addresses
    end)

    module On_identifiers :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Act_common.C_id.t =
      Travesty.Traversable.Chain0
        (struct
          type nonrec t = t

          include On_lvalues
        end)
        (Lvalue.On_identifiers)
  end
end
