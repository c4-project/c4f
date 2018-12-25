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

module Make (Lang : Basic) : S with module Lang := Lang = struct
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

  module Decl = struct
    type t =
      | Program of Lang.Program.t
      | Init    of { id : string; value : Lang.Constant.t }
      | Post of Post.t
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
      { name : string
      ; init : ((string, Lang.Constant.t) List.Assoc.t)
      ; programs : Lang.Program.t list
      } [@@deriving fields]
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
      Option.value_map
        ~default:(V.pass)
        ~f:dup_to_err
        dup

    let program_length p =
      List.length (Lang.Program.listing p)

    let is_uniform = function
      | [] -> true
      | p::ps ->
        let l = program_length p in
        List.for_all ~f:(fun p' -> program_length p' = l) ps
    ;;

    let validate_name =
      let module V = Validate in
      V.booltest (Fn.non String.is_empty) ~if_false:"name is empty"

    (** [validate_programs ps] validates an incoming litmus test's
        programs. *)
    let validate_programs : Lang.Program.t list Validate.check =
      let module V = Validate in
      V.all
        [ V.booltest (Fn.non List.is_empty) ~if_false:"programs are empty"
        ; V.booltest is_uniform ~if_false:"programs must be uniform"
        ]
    ;;

    let validate_inner t =
      let module V = Validate in
      let w check = V.field_folder t check in
      V.of_list
        (Fields.fold ~init:[]
           ~name:(w validate_name)
           ~init:(w validate_init)
           ~programs:(w validate_programs)
        )
    ;;

    let validate lit : t Or_error.t =
      Validate.valid_or_error lit validate_inner
    ;;

    let make ~name ~init ~programs =
      validate (Fields.create ~name ~init ~programs)
    ;;
  end
end
