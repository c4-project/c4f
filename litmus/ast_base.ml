(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

include Ast_base_intf

module Id = struct
  (* Comparable.Make_plain depends on Sexpable, and
     Sexpable.Of_stringable depends on Stringable.  As a result, we
     have to implement Id by snowballing together increasingly
     elaborate modules, adding Core_kernel extensions as we go. *)

  module M_str = struct
    type t =
      | Local of My_quickcheck.Small_non_negative_int.t * C_identifier.t
      | Global of C_identifier.t
    [@@deriving compare, variants, quickcheck]
    ;;

    let to_string : t -> string = function
      | Local (t, id) -> sprintf "%d:%s" t (C_identifier.to_string id)
      | Global id -> (C_identifier.to_string id)
    ;;

    let try_parse_local (s : string) : (int * string) option =
      let open Option.Let_syntax in
      let%bind (thread, rest) = String.lsplit2 ~on:':' s in
      let%bind tnum = Caml.int_of_string_opt thread in
      let%map  tnum = Option.some_if (Int.is_non_negative tnum) tnum in
      (tnum, rest)
    ;;

    let try_parse (s : string) : t Or_error.t =
      match try_parse_local s with
      | Some (t, id) -> Or_error.(id |> C_identifier.create >>| local t)
      | None -> Or_error.(s |> C_identifier.create >>| global)
    ;;

    let of_string (s : string) : t = Or_error.ok_exn (try_parse s)
  end

  module M_sexp = struct
    include M_str
    include Sexpable.Of_stringable (M_str)
  end

  include M_sexp
  include Comparable.Make (M_sexp)

  let to_memalloy_id_inner (t : int) (id : C_identifier.t)
    : string =
    sprintf "t%d%s" t (C_identifier.to_string id)
  ;;

  let%test_unit "to_memalloy_id_inner produces valid identifiers" =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = My_quickcheck.Small_non_negative_int.t * C_identifier.t
        [@@deriving sexp, quickcheck]
      end)
      ~f:(fun (t, id) ->
          [%test_pred: C_identifier.t Or_error.t]
            ~here:[[%here]]
            Or_error.is_ok
            (C_identifier.create (to_memalloy_id_inner t id))
        )

  let to_memalloy_id : t -> C_identifier.t = function
    | Local (t, id) ->
      C_identifier.of_string (to_memalloy_id_inner t id)
    | Global id -> id
  ;;
end

let%test_module "Id tests" = (module struct
  let%test_unit "to_string->of_string is identity" =
    Base_quickcheck.Test.run_exn (module Id)
      ~f:(fun ident ->
          [%test_eq: Id.t] ~here:[[%here]] ident (Id.of_string (Id.to_string ident))
        )

  let%test_unit "to_memalloy_id is identity on globals" =
    Base_quickcheck.Test.run_exn  (module C_identifier)
      ~f:(fun ident ->
          [%test_eq: C_identifier.t] ~here:[[%here]]
            ident (Id.to_memalloy_id (Global ident))
        )
end)

module Pred_elt = struct
  type 'const t =
    | Eq of Id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck, variants]

  let (==?) = eq

  module On_constants : Travesty.Traversable.S1_container
    with type 'const t := 'const t =
    Travesty.Traversable.Make_container1 (struct
      type nonrec 'const t = 'const t
      module On_monad (M : Monad.S) = struct
        module H = Travesty.Traversable.Helpers (M)

        let map_m (t : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t = Variants.map t
            ~eq:(fun v id c -> M.(c |> f >>| v.constructor id))
        ;;
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

  let (||) (l : 'const t) (r : 'const t) : 'const t = Or (l, r)
  let (&&) (l : 'const t) (r : 'const t) : 'const t = And (l, r)

  let rec debracket : 'const t -> 'const t = function
    | Bracket x  -> debracket x
    | Or  (l, r) -> Or  (debracket l, debracket r)
    | And (l, r) -> And (debracket l, debracket r)
    | Elt x      -> Elt x
  ;;

  module On_constants : Travesty.Traversable.S1_container
    with type 'const t := 'const t =
    Travesty.Traversable.Make_container1 (struct
      type nonrec 'const t = 'const t

      module On_monad (M : Monad.S) = struct
        module Ma = Applicative.Of_monad (M)
        module Elt = Pred_elt.On_constants.On_monad (M)

        let rec map_m (t : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
          Variants.map t
            ~bracket:Ma.(fun v r -> return v.constructor <*> map_m ~f r)
            ~or_:Ma.(fun v l r -> return v.constructor <*> (map_m ~f l) <*> (map_m ~f r))
            ~and_:Ma.(fun v l r -> return v.constructor <*> (map_m ~f l) <*> (map_m ~f r))
            ~elt:Ma.(fun v e -> return v.constructor <*> Elt.map_m ~f e)
      end
    end)

  module Q : Quickcheck.S1 with type 'const t := 'const t = struct
    module G = Quickcheck.Generator
    module O = Quickcheck.Observer
    module S = Quickcheck.Shrinker

    let anonymise = function
      | Bracket x  -> `A x
      | Or (l, r)  -> `B ((l, r))
      | And (l, r) -> `C ((l, r))
      | Elt x      -> `D x
    ;;

    let deanonymise = function
      | `A x        -> Bracket x
      | `B ((l, r)) -> Or (l, r)
      | `C ((l, r)) -> And (l, r)
      | `D x        -> Elt x
    ;;

    let quickcheck_generator (elt : 'const G.t) : 'const t G.t =
      G.recursive_union
        [ G.map ~f:(fun x -> Elt x) [%quickcheck.generator: [%custom elt] Pred_elt.t] ]
        ~f:(fun mu ->
            [ G.map ~f:deanonymise
                [%quickcheck.generator:
                  [ `A of [%custom mu]
                  | `B of [%custom mu] * [%custom mu]
                  | `C of [%custom mu] * [%custom mu]
                  ]
                ]
            ]
          )
    ;;

    let quickcheck_observer (elt : 'const O.t) : 'const t O.t =
      O.fixed_point
        (fun mu ->
           O.unmap ~f:anonymise
             [%quickcheck.observer:
               [ `A of [%custom mu]
               | `B of [%custom mu] * [%custom mu]
               | `C of [%custom mu] * [%custom mu]
               | `D of [%custom elt] Pred_elt.t
               ]
             ]
        )
    ;;

    let quickcheck_shrinker (elt : 'const S.t) : 'const t S.t =
      S.fixed_point
        (fun mu ->
           S.map ~f:deanonymise ~f_inverse:anonymise
             [%quickcheck.shrinker:
               [ `A of [%custom mu]
               | `B of [%custom mu] * [%custom mu]
               | `C of [%custom mu] * [%custom mu]
               | `D of [%custom elt] Pred_elt.t
               ]
             ]
        )
    ;;
  end
  include Q
end
