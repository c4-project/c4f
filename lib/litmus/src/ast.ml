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
open Base_quickcheck
module Tx = Travesty_base_exts
module Ac = Act_common

module Init = struct
  type 'const elt = {id: Ac.C_id.t; value: 'const}
  [@@deriving sexp, quickcheck]

  type 'const t = 'const elt list [@@deriving sexp, quickcheck]
end

module Decl = struct
  type ('const, 'prog) t =
    | Program of 'prog
    | Init of 'const Init.t
    | Post of 'const Postcondition.t
    | Locations of Ac.C_id.t list
  [@@deriving sexp, variants]

  let as_program : (_, 'prog) t -> 'prog option =
    Variants.map
      ~program:(fun _ -> Option.some)
      ~init:(fun _ _ -> None)
      ~post:(fun _ _ -> None)
      ~locations:(fun _ _ -> None)

  let as_init : ('const, _) t -> 'const Init.t option =
    Variants.map
      ~program:(fun _ _ -> None)
      ~init:(fun _ -> Option.some)
      ~post:(fun _ _ -> None)
      ~locations:(fun _ _ -> None)

  let as_post : ('const, _) t -> 'const Postcondition.t option =
    Variants.map
      ~program:(fun _ _ -> None)
      ~init:(fun _ _ -> None)
      ~post:(fun _ -> Option.some)
      ~locations:(fun _ _ -> None)

  let as_locations : (_, _) t -> Ac.C_id.t list option =
    Variants.map
      ~program:(fun _ _ -> None)
      ~init:(fun _ _ -> None)
      ~post:(fun _ _ -> None)
      ~locations:(fun _ -> Option.some)
end

type ('const, 'prog) t =
  {language: Ac.C_id.t; name: string; decls: ('const, 'prog) Decl.t list}
[@@deriving sexp, fields]

(** [M] allows AST type creation through referring to an existing language
    module. *)
module M (B : sig
  module Constant : T

  module Program : T
end) =
struct
  type nonrec t = (B.Constant.t, B.Program.t) t
end

let get_programs : (_, 'prog) Decl.t list -> 'prog list =
  List.filter_map ~f:Decl.as_program

let get_init (decls : ('const, _) Decl.t list) :
    (Ac.C_id.t, 'const) List.Assoc.t Or_error.t =
  Or_error.(
    decls
    |> List.filter_map ~f:Decl.as_init
    |> Tx.List.one
    >>| List.map ~f:(fun {Init.id; value} -> (id, value)))

let get_post (decls : ('const, _) Decl.t list) :
    'const Postcondition.t option Or_error.t =
  decls |> List.filter_map ~f:Decl.as_post |> Tx.List.at_most_one

let get_locations (decls : (_, _) Decl.t list) :
    Ac.C_id.t list option Or_error.t =
  decls |> List.filter_map ~f:Decl.as_locations |> Tx.List.at_most_one

let get_aux (decls : ('const, _) Decl.t list) : 'const Aux.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind init = get_init decls in
    let%bind postcondition = get_post decls in
    let%map locations = get_locations decls in
    Aux.make ~init ?postcondition ?locations ())
