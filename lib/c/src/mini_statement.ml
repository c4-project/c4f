(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Address = Mini_address
module Assign = Mini_assign
module Atomic_cmpxchg = Mini_atomic_cmpxchg
module Atomic_store = Mini_atomic_store
module Expression = Mini_expression
module Identifier = Act_c_lang.Ast_basic.Identifier
module Lvalue = Mini_lvalue

module P_statement = struct
  type 'i t =
    | Assign of Assign.t
    | Atomic_store of Atomic_store.t
    | Atomic_cmpxchg of Atomic_cmpxchg.t
    | Nop
    | If_stm of 'i
  [@@deriving sexp, variants]
end

type statement = if_statement P_statement.t

and if_statement =
  {cond: Expression.t; t_branch: statement list; f_branch: statement list}
[@@deriving sexp, fields]

module Ifs_base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)
  module O = Tx.Option.On_monad (M)

  let bmap (if_stm : if_statement) ~(cond : Expression.t F.traversal)
      ~(t_branch : statement list F.traversal)
      ~(f_branch : statement list F.traversal) : if_statement M.t =
    Fields_of_if_statement.fold ~init:(M.return if_stm)
      ~cond:(F.proc_field cond) ~t_branch:(F.proc_field t_branch)
      ~f_branch:(F.proc_field f_branch)
end

module Main :
  Mini_intf.S_statement
    with type address := Address.t
     and type assign := Assign.t
     and type atomic_cmpxchg := Atomic_cmpxchg.t
     and type atomic_store := Atomic_store.t
     and type identifier := Identifier.t
     and type if_stm := if_statement
     and type t = statement
     and type lvalue := Lvalue.t = struct
  type t = statement [@@deriving sexp]

  let assign = P_statement.assign

  let atomic_cmpxchg = P_statement.atomic_cmpxchg

  let atomic_store = P_statement.atomic_store

  let if_stm = P_statement.if_stm

  let nop () = P_statement.nop

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
      P_statement.Variants.map x ~assign:(F.proc_variant1 assign)
        ~atomic_cmpxchg:(F.proc_variant1 atomic_cmpxchg)
        ~atomic_store:(F.proc_variant1 atomic_store)
        ~if_stm:(F.proc_variant1 if_stm) ~nop:(F.proc_variant0 nop)
  end

  let map x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
    P_statement.Variants.map x ~assign:(Fn.const assign)
      ~atomic_store:(Fn.const atomic_store)
      ~atomic_cmpxchg:(Fn.const atomic_cmpxchg) ~if_stm:(Fn.const if_stm)
      ~nop:(fun _ -> nop ())

  (* We have to unroll the map over if statements here, because otherwise we
     end up with unsafe module recursion. *)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module L = Tx.List.On_monad (M)
      module Asn = Assign.On_lvalues.On_monad (M)
      module Sto = Atomic_store.On_lvalues.On_monad (M)
      module Cxg = Atomic_cmpxchg.On_lvalues.On_monad (M)

      let rec map_m x ~f =
        B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
          ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
          ~nop:M.return

      and map_m_ifs x ~f =
        IB.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(map_m ~f))
          ~f_branch:(L.map_m ~f:(map_m ~f))
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Address

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module E = Expression.On_addresses.On_monad (M)
      module L = Tx.List.On_monad (M)
      module Asn = Assign.On_addresses.On_monad (M)
      module Sto = Atomic_store.On_addresses.On_monad (M)
      module Cxg = Atomic_cmpxchg.On_addresses.On_monad (M)

      let rec map_m x ~f =
        B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
          ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
          ~nop:M.return

      and map_m_ifs x ~f =
        IB.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(map_m ~f))
          ~f_branch:(L.map_m ~f:(map_m ~f))
    end
  end)

  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)
end

module If :
  Mini_intf.S_if_statement
    with type expr := Expression.t
     and type stm := Main.t
     and type t = if_statement
     and type address := Address.t
     and type identifier := Identifier.t
     and type lvalue := Lvalue.t = struct
  type t = if_statement [@@deriving sexp]

  let cond = Field.get Fields_of_if_statement.cond

  let f_branch = Field.get Fields_of_if_statement.f_branch

  let t_branch = Field.get Fields_of_if_statement.t_branch

  let make ~cond ?(t_branch = []) ?(f_branch = []) () =
    Fields_of_if_statement.create ~cond ~t_branch ~f_branch

  module Base_map (M : Monad.S) = Ifs_base_map (M)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module S = Main.On_lvalues.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m x ~f =
        B.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(S.map_m ~f))
          ~f_branch:(L.map_m ~f:(S.map_m ~f))
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
      module S = Main.On_addresses.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m x ~f =
        B.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(S.map_m ~f))
          ~f_branch:(L.map_m ~f:(S.map_m ~f))
    end
  end)

  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t

        include On_lvalues
      end)
      (Lvalue.On_identifiers)
end

include Main
