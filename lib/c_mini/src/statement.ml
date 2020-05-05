(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module I = If
  module W = While
  module Tx = Travesty_base_exts
end

(* We can't put this into its own P module because OCaml's type system won't
   let us declare systems of recursive types where each type is an
   abbreviation. However, declaring statement inline _also_ means we can't
   use ppx. Hopefully I picked the right one out of Scylla and Charybdis. *)
type 'meta statement =
  | Prim of 'meta * Prim_statement.t
  | If_stm of 'meta if_statement
  | While_loop of 'meta while_loop
[@@deriving sexp, compare, equal]

and 'meta if_statement = ('meta, 'meta statement) If.t

and 'meta while_loop = ('meta, 'meta statement) While.t

type 'meta block = ('meta, 'meta statement) Block.t

module Main :
  Statement_types.S_statement
    with type 'meta t = 'meta statement
     and type 'meta if_stm := 'meta if_statement
     and type 'meta while_loop := 'meta while_loop = struct
  type 'meta t = 'meta statement [@@deriving sexp, compare, equal]

  module Constructors = struct
    let prim (m : 'meta) (x : Prim_statement.t) : 'meta t = Prim (m, x)

    let if_stm (x : 'meta if_statement) : 'meta t = If_stm x

    let while_loop (x : 'meta while_loop) : 'meta t = While_loop x
  end

  include Constructors

  let reduce_step (type meta result) (x : meta t)
      ~(prim : meta * Prim_statement.t -> result)
      ~(if_stm : meta if_statement -> result)
      ~(while_loop : meta while_loop -> result) : result =
    match x with
    | Prim (m, x) ->
        prim (m, x)
    | If_stm x ->
        if_stm x
    | While_loop x ->
        while_loop x

  (** Shorthand for lifting a predicate on primitives. *)
  let is_prim_and (t : 'meta t) ~(f : Prim_statement.t -> bool) : bool =
    reduce_step t ~while_loop:(Fn.const false) ~if_stm:(Fn.const false)
      ~prim:(fun (_, p) -> f p)

  (** Shorthand for writing a predicate that is [false] on primitives. *)
  let is_not_prim_and (type meta) ~(while_loop : meta while_loop -> bool)
      ~(if_stm : meta if_statement -> bool) : meta t -> bool =
    reduce_step ~while_loop ~if_stm ~prim:(Fn.const false)

  let true_of_any_if_branch_stm (m : 'meta if_statement)
      ~(predicate : 'meta t -> bool) : bool =
    List.exists
      If.(Block.statements (t_branch m) @ Block.statements (f_branch m))
      ~f:predicate

  let is_if_statement (m : 'meta t) : bool =
    is_not_prim_and m ~if_stm:(Fn.const true) ~while_loop:(Fn.const false)

  let rec has_if_statements (m : 'meta t) : bool =
    is_not_prim_and m ~if_stm:(Fn.const true) ~while_loop:(fun w ->
        List.exists (Block.statements (While.body w)) ~f:has_if_statements)

  let rec has_while_loops (m : 'meta t) : bool =
    is_not_prim_and m ~while_loop:(Fn.const true)
      ~if_stm:(true_of_any_if_branch_stm ~predicate:has_while_loops)

  let has_blocks_with_metadata (m : 'meta t) ~(predicate : 'meta -> bool) :
      bool =
    let rec mu x =
      is_not_prim_and x
        ~while_loop:(fun w ->
          let b = While.body w in
          predicate (Block.metadata b)
          || List.exists (Block.statements b) ~f:mu)
        ~if_stm:(fun ifs ->
          predicate (Block.metadata If.(t_branch ifs))
          || predicate (Block.metadata If.(f_branch ifs))
          || true_of_any_if_branch_stm ifs ~predicate:mu)
    in
    mu m

  module Base_map (M : Monad.S) = struct
    let bmap (type m1 m2) (x : m1 t)
        ~(prim : m1 * Prim_statement.t -> (m2 * Prim_statement.t) M.t)
        ~(if_stm : m1 if_statement -> m2 if_statement M.t)
        ~(while_loop : m1 while_loop -> m2 while_loop M.t) : m2 t M.t =
      Travesty_base_exts.Fn.Compose_syntax.(
        reduce_step x
          ~prim:(prim >> M.map ~f:(fun (m, x) -> Constructors.prim m x))
          ~if_stm:(if_stm >> M.map ~f:Constructors.if_stm)
          ~while_loop:(while_loop >> M.map ~f:Constructors.while_loop))

    module A = struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end

    module IB = If.Base_map (A)
    module WB = While.Base_map (A)
    module Bk = Block.On_monad (M)

    let bmap_flat (type m1 m2) (x : m1 t)
        ~(prim : m1 * Prim_statement.t -> (m2 * Prim_statement.t) M.t)
        ~(cond : Expression.t -> Expression.t M.t)
        ~(block_meta : m1 -> m2 M.t) : m2 t M.t =
      let rec mu x =
        bmap x ~prim
          ~if_stm:
            (IB.bmap ~cond
               ~t_branch:(Bk.bi_map_m ~left:block_meta ~right:mu)
               ~f_branch:(Bk.bi_map_m ~left:block_meta ~right:mu))
          ~while_loop:
            (WB.bmap ~cond
               ~body:(Bk.bi_map_m ~left:block_meta ~right:mu)
               ~kind:M.return)
      in
      mu x
  end

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module A = struct
        type 'a t = 'a M.t

        include Applicative.Of_monad (M)
      end

      module B = Base_map (M)
      module IB = If.Base_map (A)
      module WB = While.Base_map (A)
      module Bk = Block.On_monad (M)

      let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap_flat x
          ~prim:(fun (m, p) -> M.(m |> f >>| fun m' -> (m', p)))
          ~cond:M.return ~block_meta:f
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

      module P :
        Travesty.Traversable_types.S0
          with type t := Prim_statement.t
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
        module PM = Basic.P.On_monad (M)
        module EM = Basic.E.On_monad (M)

        let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
          SBase.bmap_flat x
            ~prim:(fun (m, p) -> M.(p |> PM.map_m ~f >>| fun p' -> (m, p')))
            ~cond:(EM.map_m ~f) ~block_meta:M.return
      end
    end)

    module On_lvalues :
      Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
    Make_traversal (struct
      module Elt = Lvalue
      module E = Expression_traverse.On_lvalues
      module P = Prim_statement.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression_traverse.On_addresses
      module P = Prim_statement.On_addresses
    end)

    module On_expressions :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Expression.t = Make_traversal (struct
      module Elt = Expression
      module E =
        Travesty.Traversable.Fix_elt
          (Travesty_containers.Singleton)
          (Expression)
      module P = Prim_statement.On_expressions
    end)

    module On_primitives :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Prim_statement.t = Make_traversal (struct
      module Elt = Prim_statement
      module E = Travesty.Traversable.Const (Expression) (Prim_statement)
      module P =
        Travesty.Traversable.Fix_elt
          (Travesty_containers.Singleton)
          (Prim_statement)
    end)
  end
end

include Main

module If :
  Statement_types.S_if_statement
    with type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta if_statement = struct
  type 'meta t = 'meta if_statement [@@deriving sexp, compare, equal]

  let cond = If.cond

  let f_branch = If.f_branch

  let t_branch = If.t_branch

  let make = If.make

  module Base_map (M : Monad.S) = If.Base_map (struct
    type 'a t = 'a M.t

    include Applicative.Of_monad (M)
  end)

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
          with type t := Meta.t Main.t
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
      Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
    Make_traversal (struct
      module Elt = Lvalue
      module E = Expression_traverse.On_lvalues
      module S = Sm.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression_traverse.On_addresses
      module S = Sm.On_addresses
    end)

    module On_expressions :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Expression.t = Make_traversal (struct
      module Elt = Expression
      module E =
        Travesty.Traversable.Fix_elt
          (Travesty_containers.Singleton)
          (Expression)
      module S = Sm.On_expressions
    end)

    module On_primitives :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Prim_statement.t = Make_traversal (struct
      module Elt = Prim_statement
      module E = Travesty.Traversable.Const (Expression) (Prim_statement)
      module S = Sm.On_primitives
    end)
  end
end

module While :
  Statement_types.S_while_loop
    with type 'meta expr := Expression.t
     and type 'meta stm := 'meta Main.t
     and type 'meta t = 'meta while_loop = struct
  type 'meta t = 'meta while_loop [@@deriving sexp, compare, equal]

  let cond = While.cond

  let body = While.body

  let kind = While.kind

  let make = While.make

  module Base_map (M : Monad.S) = While.Base_map (struct
    type 'a t = 'a M.t

    include Applicative.Of_monad (M)
  end)

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
          ~kind:M.return
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
          with type t := Meta.t Main.t
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
          WBase.bmap x ~cond:(EM.map_m ~f)
            ~body:(Bk.map_m ~f:(SM.map_m ~f))
            ~kind:M.return
      end
    end)

    module On_lvalues :
      Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
    Make_traversal (struct
      module Elt = Lvalue
      module E = Expression_traverse.On_lvalues
      module S = Sm.On_lvalues
    end)

    module On_addresses :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Address.t = Make_traversal (struct
      module Elt = Address
      module E = Expression_traverse.On_addresses
      module S = Sm.On_addresses
    end)

    module On_expressions :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Expression.t = Make_traversal (struct
      module Elt = Expression
      module E =
        Travesty.Traversable.Fix_elt
          (Travesty_containers.Singleton)
          (Expression)
      module S = Sm.On_expressions
    end)

    module On_primitives :
      Travesty.Traversable_types.S0
        with type t = t
         and type Elt.t = Prim_statement.t = Make_traversal (struct
      module Elt = Prim_statement
      module E = Travesty.Traversable.Const (Expression) (Prim_statement)
      module S = Sm.On_primitives
    end)
  end
end

let reduce (type meta result) (x : meta t)
    ~(prim : meta * Prim_statement.t -> result)
    ~(if_stm : (meta, result) I.t -> result)
    ~(while_loop : (meta, result) W.t -> result) : result =
  let rec mu x =
    let block_map (x : meta block) : (meta, result) Block.t =
      Block.map_right x ~f:mu
    in
    Tx.Fn.Compose_syntax.(
      reduce_step x ~prim
        ~if_stm:
          ( I.map ~cond:Fn.id ~t_branch:block_map ~f_branch:block_map
          >> if_stm )
        ~while_loop:
          (W.map ~cond:Fn.id ~body:block_map ~kind:Fn.id >> while_loop))
  in
  mu x
