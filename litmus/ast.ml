(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

open Base

include Ast_intf

module Make (Lang : Basic) : S with module Lang = Lang = struct
  module Lang = Lang

  module Id = struct
    type t =
      | Local of int * string
      | Global of string
    [@@deriving sexp]
    ;;
  end

  module Pred = struct
    type t =
      | Bracket of t
      | Or of t * t
      | And of t * t
      | Eq of Id.t * Lang.Constant.t
    [@@deriving sexp]
    ;;
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
