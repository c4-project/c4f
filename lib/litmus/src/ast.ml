(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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

let get_header (name : string) (decls : ('const, _) Decl.t list) :
    'const Header.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind init = get_init decls in
    let%bind postcondition = get_post decls in
    let%map locations = get_locations decls in
    Header.make ~name ~init ?postcondition ?locations ())
