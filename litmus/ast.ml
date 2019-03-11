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

module Make (Lang : Basic) : S with module Lang = Lang = struct
  module Lang = Lang

  module Id = Ast_base.Id

  module Pred_elt = struct
    type t = Lang.Constant.t Ast_base.Pred_elt.t
    [@@deriving sexp, compare, equal, quickcheck]

    let (==?)   = Ast_base.Pred_elt.(==?)
  end

  module Pred = struct
    type t = Lang.Constant.t Ast_base.Pred.t
    [@@deriving sexp, compare, equal]

    let bracket   = Ast_base.Pred.bracket
    let debracket = Ast_base.Pred.debracket
    let (||)      = Ast_base.Pred.(||)
    let (&&)      = Ast_base.Pred.(&&)
    let elt       = Ast_base.Pred.elt

    let rec of_blang : Pred_elt.t Blang.t -> t Or_error.t = function
      | And (l, r) ->
        let open Or_error.Let_syntax in
        let%map l' = of_blang l
        and     r' = of_blang r
        in Ast_base.Pred.And (l', r')
      | Or (l, r) ->
        let open Or_error.Let_syntax in
        let%map l' = of_blang l
        and     r' = of_blang r
        in Ast_base.Pred.Or (l', r')
      | Base x -> Or_error.return (Ast_base.Pred.Elt x)
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

    module Q : Quickcheck.S with type t := t = struct
      let quickcheck_generator =
        [%quickcheck.generator: Lang.Constant.t Ast_base.Pred.t]
      ;;
      let quickcheck_observer =
        [%quickcheck.observer: Lang.Constant.t Ast_base.Pred.t]
      ;;
      let quickcheck_shrinker =
        [%quickcheck.shrinker: Lang.Constant.t Ast_base.Pred.t]
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
                [%equal: (Lang.Type.t C_identifier.Map.t option)]
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
