(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Base_map (M : Monad.S) = struct
  (* TODO(@MattWindsor91): this should take an applicative, but can't because
     it depends on bitraversal of blocks, and on this version of Travesty
     that requires a monad.*)

  type 'meta t = 'meta Statement.t

  let bmap (type m1 m2) (x : m1 t)
      ~(prim :
            (m1, Prim_statement.t) With_meta.t
         -> (m2, Prim_statement.t) With_meta.t M.t)
      ~(if_stm : (m1, m1 t) If.t -> (m2, m2 t) If.t M.t)
      ~(flow : (m1, m1 t) Flow_block.t -> (m2, m2 t) Flow_block.t M.t) :
      m2 t M.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      Statement.reduce_step x
        ~prim:(prim >> M.map ~f:(Accessor.construct Statement.prim))
        ~if_stm:(if_stm >> M.map ~f:(Accessor.construct Statement.if_stm))
        ~flow:(flow >> M.map ~f:(Accessor.construct Statement.flow)))

  module A = Act_utils.Applicative.Of_monad_ext (M)
  module IB = If.Base_map (A)
  module FB = Flow_block.Base_map (A)
  module FE = Flow_block.Header.On_expressions.On_monad (M)
  module Bk = Block.On_monad (M)

  let bmap_flat (type m1 m2) (x : m1 t)
      ~(prim :
            (m1, Prim_statement.t) With_meta.t
         -> (m2, Prim_statement.t) With_meta.t M.t)
      ~(cond : Expression.t -> Expression.t M.t) ~(block_meta : m1 -> m2 M.t)
      : m2 t M.t =
    let rec mu x =
      let map_block = Bk.bi_map_m ~left:block_meta ~right:mu in
      bmap x ~prim
        ~if_stm:(IB.bmap ~cond ~t_branch:map_block ~f_branch:map_block)
        ~flow:(FB.bmap ~body:map_block ~header:(FE.map_m ~f:cond))
    in
    mu x
end

module On_meta :
  Travesty.Traversable_types.S1 with type 'meta t := 'meta Statement.t =
Travesty.Traversable.Make1 (struct
  type 'meta t = 'meta Statement.t

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)

    module AccM = Accessor.Of_monad (struct
      include M

      let apply = `Define_using_bind
    end)

    let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
      B.bmap_flat x
        ~prim:(AccM.map With_meta.meta ~f)
        ~cond:M.return ~block_meta:f
  end
end)

let erase_meta (type meta) (s : meta Statement.t) : unit Statement.t =
  On_meta.map s ~f:(Fn.const ())

module With_meta (Meta : T) = struct
  type nonrec t = Meta.t Statement.t

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

      module AccM = Accessor.Of_monad (struct
        include M

        let apply = `Define_using_bind
      end)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        SBase.bmap_flat x
          ~prim:(AccM.map With_meta.value ~f:(PM.map_m ~f))
          ~cond:(EM.map_m ~f) ~block_meta:M.return
    end
  end)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Make_traversal (struct
    module Elt = Lvalue
    module E =
      Travesty.Traversable.Chain0
        (Expression_traverse.On_addresses)
        (Address.On_lvalues)
    module P = Prim_statement.On_lvalues
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Make_traversal (struct
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

(** This rather expansive functor takes a method of lifting various
    sub-traversals of some [Elt] to a traversal for [Top], and instantiates
    it for a load of common values of [Elt]. *)
module Make_traversal_set
    (Meta : T)
    (Top : T)
    (F : functor
      (Basic : sig
         module Elt : Equal.S

         module E :
           Travesty.Traversable_types.S0
             with type t := Expression.t
              and module Elt = Elt

         module S :
           Travesty.Traversable_types.S0
             with type t := Meta.t Statement.t
              and module Elt = Elt
       end)
      ->
      Travesty.Traversable_types.S0
        with type t = Top.t
         and type Elt.t = Basic.Elt.t) =
struct
  module Sm = With_meta (Meta)

  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Top.t
       and type Elt.t = Lvalue.t = F (struct
    module Elt = Lvalue
    module E =
      Travesty.Traversable.Chain0
        (Expression_traverse.On_addresses)
        (Address.On_lvalues)
    module S = Sm.On_lvalues
  end)

  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Top.t
       and type Elt.t = Address.t = F (struct
    module Elt = Address
    module E = Expression_traverse.On_addresses
    module S = Sm.On_addresses
  end)

  module On_expressions :
    Travesty.Traversable_types.S0
      with type t = Top.t
       and type Elt.t = Expression.t = F (struct
    module Elt = Expression
    module E =
      Travesty.Traversable.Fix_elt
        (Travesty_containers.Singleton)
        (Expression)
    module S = Sm.On_expressions
  end)

  module On_primitives :
    Travesty.Traversable_types.S0
      with type t = Top.t
       and type Elt.t = Prim_statement.t = F (struct
    module Elt = Prim_statement
    module E = Travesty.Traversable.Const (Expression) (Prim_statement)
    module S = Sm.On_primitives
  end)
end

module If :
  Statement_types.S_traversable with type 'meta t = 'meta Statement.If.t =
struct
  type 'meta t = 'meta Statement.If.t

  module Base_map (M : Monad.S) =
    If.Base_map (Act_utils.Applicative.Of_monad_ext (M))

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
    module M = struct
      type nonrec t = Meta.t t
    end

    include M
    module Block_stms = Block.On_statements (Meta)

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
          with type t := Meta.t Statement.t
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

    include Make_traversal_set (Meta) (M) (Make_traversal)
  end
end

module Flow_block :
  Statement_types.S_traversable
    with type 'meta t = ('meta, 'meta Statement.t) Flow_block.t = struct
  type 'meta t = ('meta, 'meta Statement.t) Flow_block.t
  [@@deriving sexp, compare, equal]

  module Base_map (M : Monad.S) =
    Flow_block.Base_map (Act_utils.Applicative.Of_monad_ext (M))

  module On_meta :
    Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'meta t = 'meta t

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module Mn = On_meta.On_monad (M)
      module Bk = Block.On_monad (M)

      let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
        B.bmap x
          ~body:(Bk.bi_map_m ~left:f ~right:(Mn.map_m ~f))
          ~header:M.return
    end
  end)

  let erase_meta (type meta) (s : meta t) : unit t =
    On_meta.map s ~f:(Fn.const ())

  module With_meta (Meta : T) = struct
    module M = struct
      type nonrec t = Meta.t t
    end

    include M
    module Block_stms = Block.On_statements (Meta)

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
          with type t := Meta.t Statement.t
           and module Elt = Elt
    end) =
    Travesty.Traversable.Make0 (struct
      type nonrec t = t

      module Elt = Basic.Elt

      module On_monad (M : Monad.S) = struct
        module FBase = Base_map (M)
        module FHE = Flow_block.Header.On_expressions.On_monad (M)
        module Bk = Block_stms.On_monad (M)
        module EM = Basic.E.On_monad (M)
        module SM = Basic.S.On_monad (M)

        let map_m x ~f =
          FBase.bmap x
            ~header:(FHE.map_m ~f:(EM.map_m ~f))
            ~body:(Bk.map_m ~f:(SM.map_m ~f))
      end
    end)

    include Make_traversal_set (Meta) (M) (Make_traversal)
  end
end
