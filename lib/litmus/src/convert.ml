(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = C4f_common
module Tx = Travesty_base_exts

module Make (B : sig
  module From : Test_types.S

  module To : Test_types.S

  val constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t

  val program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t
end) =
struct
  let convert_threads :
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
         B.From.Lang.Constant.t Predicate.t
      -> B.To.Lang.Constant.t Predicate.t Or_error.t =
    Predicate.With_errors.map_right_m ~f:B.constant

  let convert_post (post : B.From.Lang.Constant.t Postcondition.t) :
      B.To.Lang.Constant.t Postcondition.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map predicate = convert_pred (Postcondition.predicate post) in
    Postcondition.make ~quantifier:(Postcondition.quantifier post) ~predicate

  let convert_post_opt :
         B.From.Lang.Constant.t Postcondition.t option
      -> B.To.Lang.Constant.t Postcondition.t option Or_error.t =
    Tx.Option.With_errors.map_m ~f:convert_post

  let convert_header (old : B.From.Lang.Constant.t Header.t) :
      B.To.Lang.Constant.t Header.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind init = convert_init (Header.init old) in
      let%map postcondition = convert_post_opt (Header.postcondition old) in
      Header.make ~name:(Header.name old) ~init ?postcondition
        ?locations:(Header.locations old) ())

  let convert (old : B.From.t) : B.To.t Or_error.t =
    let old_header = B.From.header old in
    let old_threads = B.From.threads old in
    Or_error.Let_syntax.(
      let%bind header = convert_header old_header
      and threads = convert_threads old_threads in
      B.To.make ~header ~threads)
end
