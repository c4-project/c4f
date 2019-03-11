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

include Ast_intf

module Primitives = struct
  module Id : S_id = struct
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
end

module Make (Lang : Basic) : S with module Lang = Lang = struct
  module Lang = Lang

  module Id = Primitives.Id

  module Pred_elt = struct
    type t =
      | Eq of Id.t * Lang.Constant.t
    [@@deriving sexp, compare, eq, quickcheck]
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

    module Q : Quickcheck.S with type t := t = struct
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

      let quickcheck_generator : t G.t =
        G.recursive_union
          [ G.map ~f:(fun x -> Elt x) [%quickcheck.generator: Pred_elt.t] ]
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

      let quickcheck_observer : t O.t =
        O.fixed_point
          (fun mu ->
             O.unmap ~f:anonymise
               [%quickcheck.observer:
                 [ `A of [%custom mu]
                 | `B of [%custom mu] * [%custom mu]
                 | `C of [%custom mu] * [%custom mu]
                 | `D of Pred_elt.t
                 ]
               ]
          )
      ;;

      let quickcheck_shrinker : t S.t =
        S.fixed_point
          (fun mu ->
             S.map ~f:deanonymise ~f_inverse:anonymise
               [%quickcheck.shrinker:
                 [ `A of [%custom mu]
                 | `B of [%custom mu] * [%custom mu]
                 | `C of [%custom mu] * [%custom mu]
                 | `D of Pred_elt.t
                 ]
               ]
          )
      ;;
    end
    include Q
  end

  module Post = struct
    type t =
      { quantifier : [ `Exists ]
      ; predicate  : Pred.t
      }
    [@@deriving sexp, quickcheck]
    ;;
  end

  module Init = struct
    type elt = { id : C_identifier.t; value : Lang.Constant.t }
    [@@deriving sexp, quickcheck]

    type t = elt list [@@deriving sexp, quickcheck]
  end

  module Decl = struct
    type t =
      | Program   of Lang.Program.t
      | Init      of Init.t
      | Post      of Post.t
      | Locations of C_identifier.t list (* not properly supported yet *)
    [@@deriving sexp]
    ;;
  end

  type t =
    { language : C_identifier.t
    ; name     : string
    ; decls    : Decl.t list
    }
  [@@deriving sexp, fields]
  ;;

  module Validated = struct
    type t =
      { name      : string
      ; init      : ((C_identifier.t, Lang.Constant.t) List.Assoc.t)
      ; locations : C_identifier.t list option
      ; programs  : Lang.Program.t list
      ; post      : Post.t option
      } [@@deriving fields, sexp]
    ;;

    (** [validate_init init] validates an incoming litmus test's
        init block. *)
    let validate_init (init : (C_identifier.t, Lang.Constant.t) List.Assoc.t) =
      let module Tr = Travesty in
      let module V = Validate in
      let dup =
        List.find_a_dup ~compare:(Tr.T_fn.on fst C_identifier.compare) init
      in
      let dup_to_err (k, v) =
        V.fail_s
          [%message "duplicate item in 'init'"
              ~location:(k : C_identifier.t)
              ~value:(v : Lang.Constant.t)
          ]
      in
      Option.value_map ~default:(V.pass) ~f:dup_to_err dup

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

    let validate_locations : C_identifier.t list option Validate.check =
      (* TODO(@MattWindsor91): actual validation here? *)
      Fn.const Validate.pass
    ;;

    let validate_name : string Validate.check =
      fun name ->
        if String.contains name ' '
        then Validate.fail_s
            [%message "Litmus name contains invalid character"
                ~name
            ]
        else Validate.pass
    ;;

    let validate_fields (t : t) : Validate.t =
      let w check = Validate.field_folder t check in
      Validate.of_list
        (Fields.fold ~init:[]
           ~name:(w validate_name)
           ~init:(w validate_init)
           ~programs:(w validate_programs)
           ~locations:(w validate_locations)
           ~post:(w validate_post)
        )
    ;;

    let get_uniform_globals : Lang.Program.t list ->
      Lang.Type.t C_identifier.Map.t option Or_error.t = function
      | [] -> Or_error.error_string "empty programs"
      | (x :: xs) ->
        let s = Lang.Program.global_vars x in
        let is_uniform =
          List.for_all xs
            ~f:(fun x' ->
                [%compare.equal: (Lang.Type.t C_identifier.Map.t option)]
                  s (Lang.Program.global_vars x')
              )
        in
        Travesty.T_or_error.(
          unless_m is_uniform
            ~f:(fun () -> Or_error.error_string
                   "Programs disagree on global variables sets.")
          >>| fun () -> s
        )
    ;;

    let check_init_against_globals
        (init : (C_identifier.t, Lang.Constant.t) List.Assoc.t)
        (globals : (Lang.Type.t C_identifier.Map.t))
        : unit Or_error.t =
      let init_keys =
        init |> List.map ~f:fst |> C_identifier.Set.of_list
      in
      let globals_keys =
        globals |> C_identifier.Map.keys |> C_identifier.Set.of_list
      in
      Travesty.T_or_error.unless_m
        (C_identifier.Set.equal init_keys globals_keys)
        ~f:(fun () ->
           Or_error.error_s
             [%message
               "Program global variables aren't compatible with init."
                 ~in_program:(globals_keys : C_identifier.Set.t)
                 ~in_init:(init_keys : C_identifier.Set.t)
             ]
        )
    ;;

    (** [validate_globals] checks an incoming Litmus test to ensure that,
        if its programs explicitly reference global variables, then they
        reference the same variables as both each other and the init
        block. *)
    let validate_globals : t Validate.check =
      Validate.of_error (
        fun candidate ->
          let open Or_error.Let_syntax in
          match%bind get_uniform_globals (programs candidate) with
          | None -> Result.ok_unit
          | Some gs -> check_init_against_globals (init candidate) gs
      )
    ;;

    (** [validate_post_or_location_exists] checks an incoming Litmus
       test to ensure that it has either a postcondition or a
       locations stanza. *)
    let validate_post_or_location_exists : t Validate.check =
      Validate.booltest
        (fun t ->
           Option.is_some t.locations || Option.is_some t.post)
        ~if_false:"Test must have a postcondition or location stanza."
    ;;

    let validate_location_variables : t Validate.check =
      Validate.booltest
        (fun t ->
           Option.for_all t.locations
             ~f:(List.for_all
                   ~f:(List.Assoc.mem t.init
                         ~equal:[%equal: C_identifier.t]
                      )
                )
        )
        ~if_false:"One or more locations aren't in the init."
    ;;

    let validate_inner : t Validate.check =
      Validate.all
        [ validate_fields
        ; validate_globals
        ; validate_post_or_location_exists
        ; validate_location_variables
        ]
    ;;

    let validate lit : t Or_error.t =
      Validate.valid_or_error lit validate_inner
    ;;

    let make ?locations ?post ~name ~init ~programs () =
      validate (Fields.create ~locations ~post ~name ~init ~programs)
    ;;
  end

  let get_programs (decls : Decl.t list) : Lang.Program.t list Or_error.t =
    decls
    |> List.filter_map ~f:(function Program p -> Some p | _ -> None)
    |> Or_error.return
  ;;

  let get_init (decls : Decl.t list) : (C_identifier.t, Lang.Constant.t) List.Assoc.t Or_error.t =
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

  let get_locations (decls : Decl.t list) : C_identifier.t list option Or_error.t =
    decls
    |> List.filter_map ~f:(function Locations l -> Some l | _ -> None)
    |> Travesty.T_list.at_most_one
  ;;

  let validate_language : C_identifier.t Validate.check =
    Validate.booltest
      (fun l -> String.Caseless.equal (C_identifier.to_string l) Lang.name)
      ~if_false:"incorrect language"
  ;;

  let check_language (language : C_identifier.t) =
    Validate.valid_or_error language validate_language
  ;;

  let validate ({ language; name; decls } : t) : Validated.t Or_error.t =
    let open Or_error.Let_syntax in

    let%bind programs  = get_programs   decls    in
    let%bind init      = get_init       decls    in
    let%bind post      = get_post       decls    in
    let%bind locations = get_locations  decls    in
    let%bind _         = check_language language in
    Validated.make ~name ~init ?locations ?post ~programs ()
  ;;
end

module Convert (B : Basic_convert) = struct
  let convert_programs (ps : B.From.Lang.Program.t list)
    : B.To.Lang.Program.t list Or_error.t =
    ps
    |> List.map ~f:B.program
    |> Or_error.combine_errors
  ;;

  let convert_init (init : (C_identifier.t, B.From.Lang.Constant.t) List.Assoc.t)
    : (C_identifier.t, B.To.Lang.Constant.t) List.Assoc.t Or_error.t =
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
    let name         = B.From.Validated.name      old in
    let old_init     = B.From.Validated.init      old in
    let old_post     = B.From.Validated.post      old in
    let old_programs = B.From.Validated.programs  old in
    let locations    = B.From.Validated.locations old in
    let open Or_error.Let_syntax in
    let%bind init     = convert_init old_init
    and      post     = Travesty.T_option.With_errors.map_m old_post
        ~f:convert_post
    and      programs = convert_programs old_programs
    in B.To.Validated.make ~name ~init ?post ?locations ~programs ()
end
