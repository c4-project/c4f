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

open Base
module Tx = Travesty_base_exts
open Act_utils
include Convert_intf

module Make (B : Basic) = struct
  let convert_programs :
      B.From.Lang.Program.t list -> B.To.Lang.Program.t list Or_error.t =
    Tx.Or_error.combine_map ~f:B.program

  let convert_init_line ((k, v) : C_identifier.t * B.From.Lang.Constant.t) :
      (C_identifier.t * B.To.Lang.Constant.t) Or_error.t =
    let open Or_error.Let_syntax in
    let%map v' = B.constant v in
    (k, v')

  let convert_init
      (init : (C_identifier.t, B.From.Lang.Constant.t) List.Assoc.t) :
      (C_identifier.t, B.To.Lang.Constant.t) List.Assoc.t Or_error.t =
    init |> List.map ~f:convert_init_line |> Or_error.combine_errors

  let convert_pred : B.From.Pred.t -> B.To.Pred.t Or_error.t =
    Ast_base.Pred.On_constants.With_errors.map_m ~f:B.constant

  let convert_post (post : B.From.Postcondition.t) :
      B.To.Postcondition.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map predicate = convert_pred post.predicate in
    {Ast_base.Postcondition.quantifier= post.quantifier; predicate}

  let convert_post_opt :
         B.From.Postcondition.t option
      -> B.To.Postcondition.t option Or_error.t =
    Tx.Option.With_errors.map_m ~f:convert_post

  let convert (old : B.From.Validated.t) : B.To.Validated.t Or_error.t =
    let name = B.From.Validated.name old in
    let old_init = B.From.Validated.init old in
    let old_post = B.From.Validated.postcondition old in
    let old_programs = B.From.Validated.programs old in
    let locations = B.From.Validated.locations old in
    let open Or_error.Let_syntax in
    let%bind init = convert_init old_init
    and postcondition = convert_post_opt old_post
    and programs = convert_programs old_programs in
    B.To.Validated.make ~name ~init ?postcondition ?locations ~programs ()
end
