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
module Ac = Act_common
module Tx = Travesty_base_exts

module Make (B : sig
  module From : Test_types.S

  module To : Test_types.S

  val constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t

  val program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t
end) =
struct
  let convert_programs :
      B.From.Lang.Program.t list -> B.To.Lang.Program.t list Or_error.t =
    Tx.Or_error.combine_map ~f:B.program

  let convert_init_line ((k, v) : Ac.C_id.t * B.From.Lang.Constant.t) :
      (Ac.C_id.t * B.To.Lang.Constant.t) Or_error.t =
    let open Or_error.Let_syntax in
    let%map v' = B.constant v in
    (k, v')

  let convert_init (init : (Ac.C_id.t, B.From.Lang.Constant.t) List.Assoc.t)
      : (Ac.C_id.t, B.To.Lang.Constant.t) List.Assoc.t Or_error.t =
    init |> List.map ~f:convert_init_line |> Or_error.combine_errors

  let convert_pred :
         B.From.Lang.Constant.t Postcondition.Pred.t
      -> B.To.Lang.Constant.t Postcondition.Pred.t Or_error.t =
    Postcondition.Pred.With_errors.map_right_m ~f:B.constant

  let convert_post (post : B.From.Lang.Constant.t Postcondition.t) :
      B.To.Lang.Constant.t Postcondition.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map predicate = convert_pred (Postcondition.predicate post) in
    Postcondition.make
      ~quantifier:(Postcondition.quantifier post)
      ~predicate

  let convert_post_opt :
         B.From.Lang.Constant.t Postcondition.t option
      -> B.To.Lang.Constant.t Postcondition.t option Or_error.t =
    Tx.Option.With_errors.map_m ~f:convert_post

  let convert_aux (old : B.From.Lang.Constant.t Aux.t) :
      B.To.Lang.Constant.t Aux.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind init = convert_init (Aux.init old) in
      let%map postcondition = convert_post_opt (Aux.postcondition old) in
      Aux.make ~init ?postcondition ?locations:(Aux.locations old) ())

  let convert (old : B.From.t) : B.To.t Or_error.t =
    let name = B.From.name old in
    let old_aux = B.From.aux old in
    let old_programs = B.From.programs old in
    Or_error.Let_syntax.(
      let%bind aux = convert_aux old_aux
      and programs = convert_programs old_programs in
      B.To.make ~name ~aux ~programs)
end
