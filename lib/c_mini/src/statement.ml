(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module P_statement = struct
  type 'i t =
    | Assign of Assign.t
    | Atomic_store of Atomic_store.t
    | Atomic_cmpxchg of Atomic_cmpxchg.t
    | Nop
    | If_stm of 'i
  [@@deriving sexp, equal, variants]
end

type 'meta statement = 'meta if_statement P_statement.t

and 'meta block = ('meta, 'meta statement) Block.t

and 'meta if_statement =
  {cond: Expression.t; t_branch: 'meta block; f_branch: 'meta block}
[@@deriving sexp, fields, equal]

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
      {cond= cond'; t_branch= t_branch'; f_branch= f_branch'})
end

module Main :
  Types.S_statement
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta assign := Assign.t
     and type 'meta atomic_cmpxchg := Atomic_cmpxchg.t
     and type 'meta atomic_store := Atomic_store.t
     and type 'meta if_stm := 'meta if_statement
     and type 'meta t = 'meta statement = struct
  type 'meta t = 'meta statement [@@deriving sexp, equal]

  let assign = P_statement.assign

  let atomic_cmpxchg = P_statement.atomic_cmpxchg

  let atomic_store = P_statement.atomic_store

  let if_stm = P_statement.if_stm

  let nop () = P_statement.nop

  let reduce (type meta result) (x : meta t) ~assign ~atomic_cmpxchg
      ~atomic_store ~if_stm ~nop : result =
    match x with
    | Assign x ->
        assign x
    | Atomic_cmpxchg x ->
        atomic_cmpxchg x
    | Atomic_store x ->
        atomic_store x
    | If_stm x ->
        if_stm x
    | Nop ->
        nop ()

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (type m1 m2) (x : m1 t) ~assign ~atomic_cmpxchg ~atomic_store
        ~if_stm ~nop : m2 t M.t =
      Travesty_base_exts.Fn.Compose_syntax.(
        reduce x
          ~assign:(assign >> M.map ~f:P_statement.assign)
          ~atomic_cmpxchg:
            (atomic_cmpxchg >> M.map ~f:P_statement.atomic_cmpxchg)
          ~atomic_store:(atomic_store >> M.map ~f:P_statement.atomic_store)
          ~if_stm:(if_stm >> M.map ~f:P_statement.if_stm)
          ~nop:(nop >> M.map ~f:(Fn.const P_statement.nop)))
  end

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module Bk = Block.On_monad (M)

      (* We have to unroll the map over if statements here, because
         otherwise we end up with unsafe module recursion. *)

      let rec map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x ~assign:M.return ~atomic_cmpxchg:M.return
          ~atomic_store:M.return
          ~if_stm:
            (IB.bmap ~cond:M.return
               ~t_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f))
               ~f_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f)))
          ~nop:M.return
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    type nonrec t = Meta.t t

    module Block_stms = Block.On_statements (Meta)

    module On_lvalues :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Lvalue.t = Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module IB = Ifs_base_map (M)
        module E = Expression.On_lvalues.On_monad (M)
        module Bk = Block_stms.On_monad (M)
        module Asn = Assign.On_lvalues.On_monad (M)
        module Sto = Atomic_store.On_lvalues.On_monad (M)
        module Cxg = Atomic_cmpxchg.On_lvalues.On_monad (M)

        (* We also have to unroll the map over if statements here. *)

        let rec map_m x ~f =
          B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
            ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
            ~nop:M.return

        and map_m_ifs x ~f =
          IB.bmap x ~cond:(E.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(map_m ~f))
            ~f_branch:(Bk.map_m ~f:(map_m ~f))
      end
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module IB = Ifs_base_map (M)
        module E = Expression.On_addresses.On_monad (M)
        module Bk = Block_stms.On_monad (M)
        module Asn = Assign.On_addresses.On_monad (M)
        module Sto = Atomic_store.On_addresses.On_monad (M)
        module Cxg = Atomic_cmpxchg.On_addresses.On_monad (M)

        let rec map_m x ~f =
          B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
            ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
            ~nop:M.return

        and map_m_ifs x ~f =
          IB.bmap x ~cond:(E.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(map_m ~f))
            ~f_branch:(Bk.map_m ~f:(map_m ~f))
      end
    end)

    module On_identifiers :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Act_common.C_id.t =
      Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)
  end
end

module If :
  Types.S_if_statement
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta if_statement = struct
  type 'meta t = 'meta if_statement [@@deriving sexp, equal]

  let cond x = x.cond

  let f_branch x = x.f_branch

  let t_branch x = x.t_branch

  let make = Fields_of_if_statement.create

  module Base_map (M : Monad.S) = Ifs_base_map (M)

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module Mn = Main.On_meta.On_monad (M)
      module Bk = Block.On_monad (M)

      (* We have to unroll the map over if statements here, because
         otherwise we end up with unsafe module recursion. *)

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
    module Sm = Main.With_meta (Meta)

    module On_lvalues :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Lvalue.t = Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_lvalues.On_monad (M)
        module S = Sm.On_lvalues.On_monad (M)
        module Bk = Block_stms.On_monad (M)

        let map_m x ~f =
          B.bmap x ~cond:(E.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(S.map_m ~f))
            ~f_branch:(Bk.map_m ~f:(S.map_m ~f))
      end
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t := t
         and type Elt.t = Address.t = Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)
        module S = Sm.On_addresses.On_monad (M)
        module Bk = Block_stms.On_monad (M)

        let map_m x ~f =
          B.bmap x ~cond:(E.map_m ~f)
            ~t_branch:(Bk.map_m ~f:(S.map_m ~f))
            ~f_branch:(Bk.map_m ~f:(S.map_m ~f))
      end
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

include Main
