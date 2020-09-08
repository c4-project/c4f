(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor
end

module Source = struct
  type t = Dec | Inc | Expr of Expression.t
  [@@deriving accessors, sexp, compare, equal]

  let exprs : ('i, Expression.t, t, [< A.many]) A.Simple.t =
    [%accessor
      A.many
        A.Many.(
          function
          | Dec ->
              return Dec
          | Inc ->
              return Inc
          | Expr e ->
              access e >>| fun e' -> Expr e')]
end

type t = {dst: Lvalue.t; src: Source.t}
[@@deriving accessors, sexp, make, compare, equal]

let ( @= ) (dst : Lvalue.t) (src : Expression.t) : t =
  make ~dst ~src:(Expr src)

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (assign : t) ~(dst : Lvalue.t F.traversal)
      ~(src : Source.t F.traversal) : t M.t =
    M.Let_syntax.(
      let%map dst' = dst assign.dst and src' = src assign.src in
      make ~dst:dst' ~src:src')
end

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module EL =
      Travesty.Traversable.Chain0
        (Expression_traverse.On_addresses)
        (Address.On_lvalues)
    module E = EL.On_monad (M)

    module MAcc = A.Of_monad (struct
      include M

      let apply = `Define_using_bind
    end)

    let map_m x ~f =
      B.bmap x ~dst:f ~src:(MAcc.map Source.exprs ~f:(E.map_m ~f))
  end
end)

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Expression

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)

    module MAcc = A.Of_monad (struct
      include M

      let apply = `Define_using_bind
    end)

    let map_m x ~f = B.bmap x ~dst:M.return ~src:(MAcc.map Source.exprs ~f)
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Chain0
    (On_expressions)
    (Expression_traverse.On_addresses)
