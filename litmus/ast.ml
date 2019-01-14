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

include Ast_intf

module Make (Lang : Basic) : S with module Lang = Lang = struct
  module Lang = Lang

  module Id = struct
    type t =
      | Local of int * string
      | Global of string
    [@@deriving sexp, eq, compare, variants]
    ;;

    let anonymise = function
      | Local  (int, str) -> `A ((int, str))
      | Global str        -> `B str
    ;;

    let deanonymise = function
      | `A ((int, str)) -> Local (int, str)
      | `B str          -> Global str
    ;;

    let gen : t Quickcheck.Generator.t =
      let module G = Quickcheck.Generator in
      G.map ~f:deanonymise
        (G.variant2 (G.tuple2 Int.gen String.gen) String.gen)
    ;;

    let obs : t Quickcheck.Observer.t =
      let module O = Quickcheck.Observer in
      O.unmap ~f:anonymise
        (O.variant2 (O.tuple2 Int.obs String.obs) String.obs)
    ;;

    let shrinker : t Quickcheck.Shrinker.t =
      let module S = Quickcheck.Shrinker in
      S.map ~f:deanonymise ~f_inverse:anonymise
        (S.variant2 (S.tuple2 Int.shrinker String.shrinker) String.shrinker)
    ;;
  end

  module Pred_elt = struct
    type t =
      | Eq of Id.t * Lang.Constant.t
    [@@deriving sexp, compare, eq]

    module Quickcheck : Quickcheck.S with type t := t = struct
      module G = Quickcheck.Generator
      module O = Quickcheck.Observer
      module S = Quickcheck.Shrinker

      let anonymise = function
        | Eq (x, y) -> (x, y)
      ;;

      let deanonymise (x, y) = Eq (x, y)

      let gen : t G.t =
        G.map ~f:deanonymise
          (G.tuple2 Id.gen Lang.Constant.gen)
      ;;

      let obs : t O.t =
        O.unmap ~f:anonymise
          (O.tuple2 Id.obs Lang.Constant.obs)
      ;;

      let shrinker : t S.t =
        S.map ~f:deanonymise ~f_inverse:anonymise
          (S.tuple2 Id.shrinker Lang.Constant.shrinker)
      ;;
    end
    include Quickcheck
  end

  module Pred = struct
    type t =
      | Bracket of t
      | Or of t * t
      | And of t * t
      | Elt of Pred_elt.t
    [@@deriving sexp, compare, eq]
    ;;

    let rec of_blang : Pred_elt.t Blang.t -> t Or_error.t = function
      | And (l, r) ->
        let open Or_error.Let_syntax in
        let%map l' = of_blang l
        and     r' = of_blang r
        in And (l', r')
      | Or (l, r) ->
        let open Or_error.Let_syntax in
        let%map l' = of_blang l
        and     r' = of_blang r
        in Or (l', r')
      | Base x -> Or_error.return (Elt x)
      | True | False | Not _ | If _ as b ->
        Or_error.error_s
          [%message
            "This Blang element isn't supported in Litmus predicates"
              ~element:(b : Pred_elt.t Blang.t)]
    ;;

    let rec to_blang : t -> Pred_elt.t Blang.t = function
      | Bracket x -> to_blang x
      | Or (l, r) -> Blang.O.(to_blang l || to_blang r)
      | And (l, r) -> Blang.O.(to_blang l && to_blang r)
      | Elt x -> Blang.base x
    ;;

    let rec debracket : t -> t = function
      | Bracket x  -> debracket x
      | Or  (l, r) -> Or  (debracket l, debracket r)
      | And (l, r) -> And (debracket l, debracket r)
      | Elt x      -> Elt x
    ;;

    module Quickcheck : Quickcheck.S with type t := t = struct
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

      let gen : t G.t =
        G.recursive_union
          [ G.map ~f:(fun x -> Elt x) Pred_elt.gen ]
          ~f:(fun mu ->
              [ G.map ~f:(fun x -> Bracket x) mu
              ; G.map2 ~f:(fun x y -> And (x, y)) mu mu
              ; G.map2 ~f:(fun x y -> Or (x, y)) mu mu
              ])
      ;;

      let obs : t O.t =
        O.fixed_point
          (fun mu ->
             O.unmap ~f:anonymise
               (O.variant4
                  mu
                  (O.tuple2 mu mu)
                  (O.tuple2 mu mu)
                  Pred_elt.obs))
      ;;

      let shrinker : t S.t =
        S.fixed_point
          (fun mu ->
             S.map ~f:deanonymise ~f_inverse:anonymise
               (S.variant4
                  mu
                  (S.tuple2 mu mu)
                  (S.tuple2 mu mu)
                  Pred_elt.shrinker))
      ;;
    end
    include Quickcheck
  end

  module Post = struct
    type t =
      { quantifier : [ `Exists ]
      ; predicate  : Pred.t
      }
    [@@deriving sexp]
    ;;
  end

  module Init = struct
    type elt = { id : string; value : Lang.Constant.t } [@@deriving sexp]

    type t = elt list [@@deriving sexp]
  end

  module Decl = struct
    type t =
      | Program of Lang.Program.t
      | Init    of Init.t
      | Post    of Post.t
    [@@deriving sexp]
    ;;
  end

  type t =
    { language : string
    ; name     : string
    ; decls    : Decl.t list
    }
  [@@deriving sexp, fields]
  ;;

  module Validated = struct
    type t =
      { name     : string
      ; init     : ((string, Lang.Constant.t) List.Assoc.t)
      ; programs : Lang.Program.t list
      ; post     : Post.t option
      } [@@deriving fields, sexp]
    ;;

    (** [validate_init init] validates an incoming litmus test's
        init block. *)
    let validate_init (init : (string, Lang.Constant.t) List.Assoc.t) =
      let module Tr = Travesty in
      let module V = Validate in
      let dup =
        List.find_a_dup ~compare:(Tr.T_fn.on fst String.compare) init
      in
      let dup_to_err (k, v) =
        V.fail_s
          [%message "duplicate item in 'init'"
              ~location:k
              ~value:(v : Lang.Constant.t)
          ]
      in
      Option.value_map ~default:(V.pass) ~f:dup_to_err dup

    let validate_name =
      Validate.booltest (Fn.non String.is_empty) ~if_false:"name is empty"
    ;;

    (** [validate_programs ps] validates an incoming litmus test's
        programs. *)
    let validate_programs : Lang.Program.t list Validate.check =
      let module V = Validate in
      V.all
        [ V.booltest (Fn.non List.is_empty) ~if_false:"programs are empty"
        (* TODO(@MattWindsor91): duplicate name checking *)
        ]
    ;;

    let validate_post : Post.t option Validate.check =
      (* TODO(@MattWindsor91): actual validation here? *)
      Fn.const Validate.pass
    ;;

    let validate_inner t =
      let module V = Validate in
      let w check = V.field_folder t check in
      V.of_list
        (Fields.fold ~init:[]
           ~name:(w validate_name)
           ~init:(w validate_init)
           ~programs:(w validate_programs)
           ~post:(w validate_post)
        )
    ;;

    let validate lit : t Or_error.t =
      Validate.valid_or_error lit validate_inner
    ;;

    let make ?post ~name ~init ~programs () =
      validate (Fields.create ~post ~name ~init ~programs)
    ;;
  end

  let get_programs (decls : Decl.t list) : Lang.Program.t list Or_error.t =
    decls
    |> List.filter_map ~f:(function Program p -> Some p | _ -> None)
    |> Or_error.return
  ;;

  let get_init (decls : Decl.t list) : (string, Lang.Constant.t) List.Assoc.t Or_error.t =
    Or_error.(
      decls
      |>  List.filter_map ~f:(function Init p -> Some p | _ -> None)
      |>  Travesty.T_list.one
      >>| List.map ~f:(fun { Init.id; value } -> (id, value))
    )
  ;;

  let get_post (decls : Decl.t list) : Post.t option Or_error.t =
    decls
    |> List.filter_map ~f:(function Post p -> Some p | _ -> None)
    |> Travesty.T_list.at_most_one
  ;;

  let validate_language : string Validate.check =
    Validate.booltest
      (String.Caseless.equal Lang.name)
      ~if_false:"incorrect language"
  ;;

  let check_language (language : string) =
    Validate.valid_or_error language validate_language
  ;;

  let validate ({ language; name; decls } : t) : Validated.t Or_error.t =
    let open Or_error.Let_syntax in

    let%bind programs = get_programs   decls    in
    let%bind init     = get_init       decls    in
    let%bind post     = get_post       decls    in
    let%bind _        = check_language language in
    Validated.make ~name ~init ?post ~programs ()
  ;;
end

module Convert (B : Basic_convert) = struct
  let convert_programs (ps : B.From.Lang.Program.t list)
    : B.To.Lang.Program.t list Or_error.t =
    ps
    |> List.map ~f:B.program
    |> Or_error.combine_errors
  ;;

  let convert_init (init : (string, B.From.Lang.Constant.t) List.Assoc.t)
    : (string, B.To.Lang.Constant.t) List.Assoc.t Or_error.t =
    init
    |> List.map
      ~f:(fun (k, v) -> Or_error.(B.constant v >>| Tuple2.create k))
    |> Or_error.combine_errors
  ;;

  let convert_id
    : B.From.Id.t -> B.To.Id.t = function
    | Local (thr, id) -> Local (thr, id)
    | Global id -> Global id
  ;;

  let rec convert_pred
    : B.From.Pred.t -> B.To.Pred.t Or_error.t = function
    | Bracket x ->
      Or_error.(x |> convert_pred >>| fun x' -> B.To.Pred.Bracket x')
    | Or (l, r) ->
      Or_error.map2 (convert_pred l) (convert_pred r)
        ~f:(fun l' r' -> B.To.Pred.Or (l', r'))
    | And (l, r) ->
      Or_error.map2 (convert_pred l) (convert_pred r)
        ~f:(fun l' r' -> B.To.Pred.And (l', r'))
    | Elt (Eq (id, k)) ->
      let id' = convert_id id in
      Or_error.(k |> B.constant >>| fun k' -> B.To.Pred.(Elt (Eq (id', k'))))
  ;;

  let convert_post (post : B.From.Post.t) : B.To.Post.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map predicate = convert_pred (post.predicate) in
    { B.To.Post.quantifier = post.quantifier; predicate }
  ;;

  let convert (old : B.From.Validated.t) : B.To.Validated.t Or_error.t =
    let name         = B.From.Validated.name     old in
    let old_init     = B.From.Validated.init     old in
    let old_post     = B.From.Validated.post     old in
    let old_programs = B.From.Validated.programs old in
    let open Or_error.Let_syntax in
    let%bind init     = convert_init old_init
    and      post     = Travesty.T_option.With_errors.map_m old_post
        ~f:convert_post
    and      programs = convert_programs old_programs
    in B.To.Validated.make ~name ~init ?post ~programs ()
end
