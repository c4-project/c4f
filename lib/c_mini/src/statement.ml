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
  [@@deriving sexp, fields, compare, equal]
end

(** [P_while_loop] contains a fully type-parametrised while_loop type. *)
module P_while_loop = struct
  type ('meta, 'stm) t =
    { cond: Expression.t
    ; body: ('meta, 'stm) Block.t
    ; kind: [`While | `Do_while] }
  [@@deriving sexp, fields, compare, equal]
end

(* We can't put this into its own P module because OCaml's type system won't
   let us declare systems of recursive types where each type is an
   abbreviation. However, declaring statement inline _also_ means we can't
   use ppx. Hopefully I picked the right one out of Scylla and Charybdis. *)
type 'meta statement =
  | Prim of 'meta Prim_statement.t
  | If_stm of 'meta if_statement
  | While_loop of 'meta while_loop
[@@deriving sexp, compare, equal]

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
  Statement_types.S_statement
    with type address := Address.t
     and type 'meta t = 'meta statement
     and type identifier := Act_c_lang.Ast_basic.Identifier.t
     and type lvalue := Lvalue.t
     and type 'meta if_stm := 'meta if_statement
     and type 'meta while_loop := 'meta while_loop = struct
  type 'meta t = 'meta statement [@@deriving sexp, compare, equal]

  module Constructors = struct
    let prim (x : 'meta Prim_statement.t) : 'meta t = Prim x

    let assign (x : (* 'meta *) Assign.t) : 'meta t =
      prim (Prim_statement.assign x)

    let atomic (m : 'meta) (x : Atomic_statement.t) : 'meta t =
      prim (Prim_statement.atomic m x)

    let atomic_cmpxchg (m : 'meta) (x : Atomic_cmpxchg.t) : 'meta t =
      prim (Prim_statement.atomic_cmpxchg m x)

    let atomic_fence (m : 'meta) (x : Atomic_fence.t) : 'meta t =
      prim (Prim_statement.atomic_fence m x)

    let atomic_store (m : 'meta) (x : Atomic_store.t) : 'meta t =
      prim (Prim_statement.atomic_store m x)

    let break (x : 'meta) : 'meta t = prim (Prim_statement.break x)

    let continue (x : 'meta) : 'meta t = prim (Prim_statement.continue x)

    let return (x : 'meta) : 'meta t = prim (Prim_statement.return x)

    let label (x : 'meta Label.t) : 'meta t = prim (Prim_statement.label x)

    let goto (x : 'meta Label.t) : 'meta t = prim (Prim_statement.goto x)

    let nop (x : 'meta) : 'meta t = prim (Prim_statement.nop x)

    let procedure_call (x : 'meta Call.t) : 'meta t =
      prim (Prim_statement.procedure_call x)

    let if_stm (x : 'meta if_statement) : 'meta t = If_stm x

    let while_loop (x : 'meta while_loop) : 'meta t = While_loop x
  end

  include Constructors

  let reduce (type meta result) (x : meta t)
      ~(prim : meta Prim_statement.t -> result)
      ~(if_stm : meta if_statement -> result)
      ~(while_loop : meta while_loop -> result) : result =
    match x with
    | Prim x ->
        prim x
    | If_stm x ->
        if_stm x
    | While_loop x ->
        while_loop x

  (** Shorthand for writing a predicate that is [false] on primitives. *)
  let is_not_prim_and (type meta) ~(while_loop : meta while_loop -> bool)
      ~(if_stm : meta if_statement -> bool) : meta t -> bool =
    reduce ~while_loop ~if_stm ~prim:(Fn.const false)

  let true_of_any_if_branch_stm (m : 'meta if_statement)
      ~(predicate : 'meta t -> bool) : bool =
    List.exists
      (Block.statements m.t_branch @ Block.statements m.f_branch)
      ~f:predicate

  let is_if_statement (m : 'meta t) : bool =
    is_not_prim_and m ~if_stm:(Fn.const true) ~while_loop:(Fn.const false)

  let rec has_if_statements (m : 'meta t) : bool =
    is_not_prim_and m ~if_stm:(Fn.const true) ~while_loop:(fun {body; _} ->
        List.exists (Block.statements body) ~f:has_if_statements)

  let rec has_while_loops (m : 'meta t) : bool =
    is_not_prim_and m ~while_loop:(Fn.const true)
      ~if_stm:(true_of_any_if_branch_stm ~predicate:has_while_loops)

  let has_blocks_with_metadata (m : 'meta t) ~(predicate : 'meta -> bool) :
      bool =
    let rec mu x =
      is_not_prim_and x
        ~while_loop:(fun {body; _} ->
          predicate (Block.metadata body)
          || List.exists (Block.statements body) ~f:mu)
        ~if_stm:(fun ifs ->
          predicate (Block.metadata ifs.t_branch)
          || predicate (Block.metadata ifs.f_branch)
          || true_of_any_if_branch_stm ifs ~predicate:mu)
    in
    mu m

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (type m1 m2) (x : m1 t) ~prim ~if_stm ~while_loop : m2 t M.t =
      Travesty_base_exts.Fn.Compose_syntax.(
        reduce x
          ~prim:(prim >> M.map ~f:Constructors.prim)
          ~if_stm:(if_stm >> M.map ~f:Constructors.if_stm)
          ~while_loop:(while_loop >> M.map ~f:Constructors.while_loop))
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
      module PM = Prim_statement.On_meta.On_monad (M)

      (* We have to unroll the map over if/while here, because otherwise we
         end up with unsafe module recursion. *)

      let rec map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x ~prim:(PM.map_m ~f)
          ~if_stm:
            (IB.bmap ~cond:M.return
               ~t_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f))
               ~f_branch:(Bk.bi_map_m ~left:f ~right:(map_m ~f)))
          ~while_loop:
            (WB.bmap ~cond:M.return
               ~body:(Bk.bi_map_m ~left:f ~right:(map_m ~f)))
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    type nonrec t = Meta.t t

    module Prim_meta = Prim_statement.With_meta (Meta)
    module Block_stms = Block.On_statements (Meta)

    (** Does the legwork of implementing a particular type of traversal over
        statements. *)
    module Make_traversal (Basic : sig
      module Elt : Equal.S

      module P :
        Travesty.Traversable_types.S0
          with type t := Meta.t Prim_statement.t
           and module Elt = Elt

      module E :
        Travesty.Traversable_types.S0
          with type t := Expression.t
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
        module PM = Basic.P.On_monad (M)
        module EM = Basic.E.On_monad (M)

        (* We also have to unroll the map over if/while here. *)

        let rec map_m x ~f =
          SBase.bmap x ~prim:(PM.map_m ~f) ~if_stm:(map_m_ifs ~f)
            ~while_loop:(map_m_whiles ~f)

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
      module E = Expression.On_lvalues
      module P = Prim_meta.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression.On_addresses
      module P = Prim_meta.On_addresses
    end)

    module On_identifiers :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Act_common.C_id.t =
      Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)

(* TODO(@MattWindsor91): needs some refactoring
    module On_primitives : Travesty.Traversable_types.S0
      with type t = t
      and type Elt.t = Meta.t Prim_statement.t =
      Travesty.Traversable.Make0 (struct
        type nonrec t = t
        module Elt = struct
          type t = Meta.t Prim_statement.t
        end

        module On_monad (M : Monad.S) = struct
          module SBase = Base_map (M)
          module IBase = Ifs_base_map (M)
          module WBase = Whiles_base_map (M)

          module Bk = Block_stms.On_monad (M)
          let rec map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
            SBase.bmap x ~prim:f ~if_stm:(map_m_ifs ~f)
              ~while_loop:(map_m_whiles ~f)

        and map_m_ifs x ~f =
          IBase.bmap x ~cond:M.return
            ~t_branch:(Bk.map_m ~f:(map_m ~f))
            ~f_branch:(Bk.map_m ~f:(map_m ~f))

        and map_m_whiles x ~f =
          WBase.bmap x ~cond:M.return ~body:(Bk.map_m ~f:(map_m ~f))
        end
      end) *)
  end
end

include Main

module If :
  Statement_types.S_if_statement
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta if_statement = struct
  type 'meta t = 'meta if_statement [@@deriving sexp, compare, equal]

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
  Statement_types.S_while_loop
    with type address := Address.t
     and type identifier := Act_common.C_id.t
     and type lvalue := Lvalue.t
     and type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta while_loop = struct
  type 'meta t = 'meta while_loop [@@deriving sexp, compare, equal]

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
