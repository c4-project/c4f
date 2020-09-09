(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Source = struct
  type t = Dec | Inc | Expr of Expression.t
  [@@deriving accessors, sexp, compare, equal]

  let exprs' =
    Accessor.Many.(
      function
      | Dec ->
          return Dec
      | Inc ->
          return Inc
      | Expr e ->
          access e >>| fun e' -> Expr e')

  let exprs : ('i, Expression.t, t, [< many]) Accessor.Simple.t =
    (* This could literally just be [expr], but I'm planning to add more (x
       += e) and so on, which'll complicate things. *)
    [%accessor Accessor.(many exprs')]

  let anon =
    [%accessor
      Accessor.isomorphism
        ~get:(function Dec -> `A | Inc -> `B | Expr e -> `C e)
        ~construct:(function `A -> Dec | `B -> Inc | `C e -> Expr e)]

  let quickcheck_observer : t Q.Observer.t =
    Q.Observer.unmap [%quickcheck.observer: [`A | `B | `C of Expression.t]]
      ~f:(Accessor.get anon)

  module Quickcheck_generic
      (Expr : Utils.My_quickcheck.S_with_sexp with type t := Expression.t) :
    Utils.My_quickcheck.S_with_sexp with type t = t = struct
    type nonrec t = t

    let sexp_of_t = sexp_of_t

    let quickcheck_generator : t Q.Generator.t =
      Q.Generator.map
        [%quickcheck.generator:
          [`A | `B | `C of [%custom Expr.quickcheck_generator]]]
        ~f:(Accessor.construct anon)

    let quickcheck_observer : t Q.Observer.t =
      Q.Observer.unmap
        [%quickcheck.observer:
          [`A | `B | `C of [%custom Expr.quickcheck_observer]]]
        ~f:(Accessor.get anon)

    let quickcheck_shrinker : t Q.Shrinker.t =
      Q.Shrinker.map
        [%quickcheck.shrinker:
          [`A | `B | `C of [%custom Expr.quickcheck_shrinker]]]
        ~f:(Accessor.construct anon) ~f_inverse:(Accessor.get anon)
  end

  module Quickcheck_int = Quickcheck_generic

  (* Currently, all possible sources are compatible with integers. *)

  module Quickcheck_bool
      (Expr : Utils.My_quickcheck.S_with_sexp with type t := Expression.t) :
    Utils.My_quickcheck.S_with_sexp with type t = t = struct
    include Quickcheck_generic (Expr)

    let quickcheck_generator =
      Q.Generator.filter quickcheck_generator ~f:(function
        | Inc | Dec ->
            false
        | Expr _ ->
            true (* assuming Expr generates Boolean expressions! *))
  end
end

type t = {dst: Lvalue.t; src: Source.t}
[@@deriving accessors, sexp, make, compare, equal]

let ( @= ) (dst : Lvalue.t) (src : Expression.t) : t =
  make ~dst ~src:(Expr src)

let exprs : ('i, Expression.t, t, [< many]) Accessor.Simple.t =
  [%accessor
    Accessor.(
      many
        Many.(
          Let_syntax.(
            fun {dst; src} ->
              let%map src' = Source.exprs' src in
              {dst; src= src'})))]

(* We can't easily do lvalues yet, because we'd need an accessor for all the
   lvalues in an expression. *)

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

    module MAcc = Accessor.Of_monad (struct
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
    module MAcc = Accessor.Of_monad (struct
      include M

      let apply = `Define_using_bind
    end)

    let map_m x ~f = MAcc.map exprs x ~f
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Chain0
    (On_expressions)
    (Expression_traverse.On_addresses)

let anon =
  [%accessor
    Accessor.isomorphism
      ~get:(fun {dst; src} -> (dst, src))
      ~construct:(fun (dst, src) -> {dst; src})]

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Base_quickcheck.Observer.(
    unmap [%quickcheck.observer: Lvalue.t * Source.t] ~f:(Accessor.get anon))

module Quickcheck_generic
    (Src : Utils.My_quickcheck.S_with_sexp with type t := Source.t)
    (Dst : Utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) :
  Utils.My_quickcheck.S_with_sexp with type t = t = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.(
      map [%quickcheck.generator: Dst.t * Src.t] ~f:(Accessor.construct anon))

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    Base_quickcheck.Observer.(
      unmap [%quickcheck.observer: Dst.t * Src.t] ~f:(Accessor.get anon))

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.(
      map [%quickcheck.shrinker: Dst.t * Src.t] ~f:(Accessor.construct anon)
        ~f_inverse:(Accessor.get anon))
end
