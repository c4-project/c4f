(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
include Ast_base_intf

module Pred_elt = struct
  type 'const t = Eq of Id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck, variants]

  let ( ==? ) = eq

  module On_constants :
    Travesty.Traversable.S1_container with type 'const t := 'const t =
  Travesty.Traversable.Make_container1 (struct
    type nonrec 'const t = 'const t

    module On_monad (M : Monad.S) = struct
      module H = Travesty.Traversable.Helpers (M)

      let map_m (t : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        Variants.map t ~eq:(fun v id c -> M.(c |> f >>| v.constructor id))
    end
  end)
end

module Pred = struct
  type 'const t =
    | Bracket of 'const t
    | Or of 'const t * 'const t
    | And of 'const t * 'const t
    | Elt of 'const Pred_elt.t
  [@@deriving sexp, compare, equal, variants]

  let ( || ) (l : 'const t) (r : 'const t) : 'const t = Or (l, r)

  let ( && ) (l : 'const t) (r : 'const t) : 'const t = And (l, r)

  let rec debracket : 'const t -> 'const t = function
    | Bracket x ->
        debracket x
    | Or (l, r) ->
        Or (debracket l, debracket r)
    | And (l, r) ->
        And (debracket l, debracket r)
    | Elt x ->
        Elt x

  module On_constants :
    Travesty.Traversable.S1_container with type 'const t := 'const t =
  Travesty.Traversable.Make_container1 (struct
    type nonrec 'const t = 'const t

    module On_monad (M : Monad.S) = struct
      module Ma = Applicative.Of_monad (M)
      module Elt = Pred_elt.On_constants.On_monad (M)

      let rec map_m (t : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        Variants.map t
          ~bracket:Ma.(fun v r -> return v.constructor <*> map_m ~f r)
          ~or_:
            Ma.(
              fun v l r ->
                return v.constructor <*> map_m ~f l <*> map_m ~f r)
          ~and_:
            Ma.(
              fun v l r ->
                return v.constructor <*> map_m ~f l <*> map_m ~f r)
          ~elt:Ma.(fun v e -> return v.constructor <*> Elt.map_m ~f e)
    end
  end)

  module Q : Quickcheck.S1 with type 'const t := 'const t = struct
    module G = Quickcheck.Generator
    module O = Quickcheck.Observer
    module S = Quickcheck.Shrinker

    let anonymise = function
      | Bracket x ->
          `A x
      | Or (l, r) ->
          `B (l, r)
      | And (l, r) ->
          `C (l, r)
      | Elt x ->
          `D x

    let deanonymise = function
      | `A x ->
          Bracket x
      | `B (l, r) ->
          Or (l, r)
      | `C (l, r) ->
          And (l, r)
      | `D x ->
          Elt x

    let quickcheck_generator (elt : 'const G.t) : 'const t G.t =
      G.recursive_union
        [ G.map
            ~f:(fun x -> Elt x)
            [%quickcheck.generator: [%custom elt] Pred_elt.t] ]
        ~f:(fun mu ->
          [ G.map ~f:deanonymise
              [%quickcheck.generator:
                [ `A of [%custom mu]
                | `B of [%custom mu] * [%custom mu]
                | `C of [%custom mu] * [%custom mu] ]] ] )

    let quickcheck_observer (elt : 'const O.t) : 'const t O.t =
      O.fixed_point (fun mu ->
          O.unmap ~f:anonymise
            [%quickcheck.observer:
              [ `A of [%custom mu]
              | `B of [%custom mu] * [%custom mu]
              | `C of [%custom mu] * [%custom mu]
              | `D of [%custom elt] Pred_elt.t ]] )

    let quickcheck_shrinker (elt : 'const S.t) : 'const t S.t =
      S.fixed_point (fun mu ->
          S.map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker:
              [ `A of [%custom mu]
              | `B of [%custom mu] * [%custom mu]
              | `C of [%custom mu] * [%custom mu]
              | `D of [%custom elt] Pred_elt.t ]] )
  end

  include Q
end

module Postcondition = struct
  type 'const t = {quantifier: [`Exists]; predicate: 'const Pred.t}
  [@@deriving sexp, compare, equal, quickcheck, fields, make]

  module On_constants :
    Travesty.Traversable.S1_container with type 'const t := 'const t =
  Travesty.Traversable.Make_container1 (struct
    type nonrec 'const t = 'const t

    module On_monad (M : Monad.S) = struct
      module Elt = Pred.On_constants.On_monad (M)

      let map_m (t : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        Fields.fold ~init:(M.return t)
          ~quantifier:(fun xm _fld -> xm)
          ~predicate:(fun xm _fld ->
            let open M.Let_syntax in
            let%bind x = xm in
            let p = x.predicate in
            let%map p' = Elt.map_m ~f p in
            make ~quantifier:x.quantifier ~predicate:p' )
    end
  end)
end
