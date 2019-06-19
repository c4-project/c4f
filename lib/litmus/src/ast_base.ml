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

open Core_kernel (* not Base; for extended quickcheck *)

open Ast_base_intf

module Pred_elt = struct
  type 'const t = Eq of Id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck, variants]

  let ( ==? ) = eq

  module BT :
    Travesty.Bi_traversable.S1_right
    with type 'const t := 'const t
     and type left = Id.t = Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Id.t

    module On_monad (M : Monad.S) = struct
      let bi_map_m (t : 'a t) ~(left : Id.t -> Id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        match t with
        | Eq (id, c) ->
            M.Let_syntax.(
              let%map id' = left id and c' = right c in
              Eq (id', c'))
    end
  end)

  include BT

  (* TODO(@MattWindsor91): this is yet another pattern that needs putting
     into Travesty *)
  module On_c_identifiers :
    Travesty.Bi_traversable.S1_right
    with type 'const t = 'const t
     and type left = Act_common.C_id.t =
  Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Act_common.C_id.t

    module On_monad (M : Monad.S) = struct
      module Lid_cid = Act_common.Litmus_id.On_c_identifiers.On_monad (M)
      module B = On_monad (M)

      let bi_map_m (t : 'a t)
          ~(left : Act_common.C_id.t -> Act_common.C_id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        B.bi_map_m ~left:(Lid_cid.map_m ~f:left) ~right t
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

  module BT :
    Travesty.Bi_traversable.S1_right
    with type 'const t := 'const t
     and type left = Id.t = Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Id.t

    module On_monad (M : Monad.S) = struct
      module Pe = Pred_elt.On_monad (M)

      let rec bi_map_m (t : 'a t) ~(left : Id.t -> Id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        M.Let_syntax.(
          match t with
          | Bracket x ->
              let%map x' = bi_map_m ~left ~right x in
              Bracket x'
          | Or (l, r) ->
              let%map l' = bi_map_m ~left ~right l
              and r' = bi_map_m ~left ~right r in
              Or (l', r')
          | And (l, r) ->
              let%map l' = bi_map_m ~left ~right l
              and r' = bi_map_m ~left ~right r in
              And (l', r')
          | Elt x ->
              let%map x' = Pe.bi_map_m ~left ~right x in
              Elt x')
    end
  end)

  include BT

  module On_c_identifiers :
    Travesty.Bi_traversable.S1_right
    with type 'const t = 'const t
     and type left = Act_common.C_id.t =
  Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Act_common.C_id.t

    module On_monad (M : Monad.S) = struct
      module Lid_cid = Act_common.Litmus_id.On_c_identifiers.On_monad (M)
      module B = On_monad (M)

      let bi_map_m (t : 'a t)
          ~(left : Act_common.C_id.t -> Act_common.C_id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        B.bi_map_m ~left:(Lid_cid.map_m ~f:left) ~right t
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

  module BT :
    Travesty.Bi_traversable.S1_right
    with type 'const t := 'const t
     and type left = Id.t = Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Id.t

    module On_monad (M : Monad.S) = struct
      module Pr = Pred.On_monad (M)

      let bi_map_m (t : 'a t) ~(left : Id.t -> Id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        let quantifier = quantifier t in
        M.Let_syntax.(
          let%map predicate = Pr.bi_map_m ~left ~right (predicate t) in
          make ~quantifier ~predicate)
    end
  end)

  include BT

  module On_c_identifiers :
    Travesty.Bi_traversable.S1_right
    with type 'const t = 'const t
     and type left = Act_common.C_id.t =
  Travesty.Bi_traversable.Make1_right (struct
    type nonrec 'const t = 'const t

    type left = Act_common.C_id.t

    module On_monad (M : Monad.S) = struct
      module Lid_cid = Act_common.Litmus_id.On_c_identifiers.On_monad (M)
      module B = On_monad (M)

      let bi_map_m (t : 'a t)
          ~(left : Act_common.C_id.t -> Act_common.C_id.t M.t)
          ~(right : 'a -> 'b M.t) : 'b t M.t =
        B.bi_map_m ~left:(Lid_cid.map_m ~f:left) ~right t
    end
  end)
end
