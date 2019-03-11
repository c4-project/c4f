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

open Base
open Utils
include Convert_intf

module Make (B : Basic) = struct
  let convert_programs (ps : B.From.Lang.Program.t list)
    : B.To.Lang.Program.t list Or_error.t =
    ps
    |> List.map ~f:B.program
    |> Or_error.combine_errors
  ;;

  let convert_init_line ((k, v) : (C_identifier.t * B.From.Lang.Constant.t))
    : (C_identifier.t * B.To.Lang.Constant.t) Or_error.t =
    let open Or_error.Let_syntax in
    let%map v' = B.constant v in (k, v')
  ;;

  let convert_init (init : (C_identifier.t, B.From.Lang.Constant.t) List.Assoc.t)
    : (C_identifier.t, B.To.Lang.Constant.t) List.Assoc.t Or_error.t =
    init
    |> List.map ~f:convert_init_line
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
