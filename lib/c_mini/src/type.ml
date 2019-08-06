(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Au = Act_utils

module Prim = struct
  type t = Int | Bool [@@deriving variants, equal, enumerate]
end

module Basic = struct
  module M = struct
    type t = {atomic: bool; prim: Prim.t} [@@deriving equal, enumerate]

    let int ?(atomic : bool = false) () : t = {atomic; prim= Int}

    let bool ?(atomic : bool = false) () : t = {atomic; prim= Bool}

    let table : (t, string) List.Assoc.t =
      [ (int (), "int")
      ; (int ~atomic:true (), "atomic_int")
      ; (bool (), "bool")
      ; (bool ~atomic:true (), "atomic_bool") ]
  end

  module M_enum = struct
    include M
    include Au.Enum.Make_from_enumerate (M)
  end

  include M
  include Au.Enum.Extend_table (M_enum)

  let to_spec : t -> [> Act_c_lang.Ast.Type_spec.t] = function
    | {atomic= false; prim= Int} ->
        `Int
    | {atomic= true; prim= Int} ->
        `Defined_type (Ac.C_id.of_string "atomic_int")
    | {atomic= false; prim= Bool} ->
        `Defined_type (Ac.C_id.of_string "bool")
    | {atomic= true; prim= Bool} ->
        `Defined_type (Ac.C_id.of_string "atomic_bool")

  let strip_atomic (t : t) : t = {t with atomic= false}

  let to_non_atomic : t -> t Or_error.t = function
    | {atomic= true; prim} ->
        Or_error.return {atomic= false; prim}
    | _ ->
        Or_error.error_string "already non-atomic"

  let is_atomic ({atomic; _} : t) : bool = atomic
end

module M1 = struct
  type t = Normal of Basic.t | Pointer_to of Basic.t
  [@@deriving variants, equal, compare, quickcheck]

  let of_string (s : string) : t =
    match String.chop_suffix s ~suffix:"*" with
    | None ->
        Normal (Basic.of_string s)
    | Some s' ->
        Pointer_to (Basic.of_string s')

  let to_string : t -> string = function
    | Normal b ->
        Basic.to_string b
    | Pointer_to b ->
        Basic.to_string b ^ "*"
end

module M = struct
  include M1
  include Sexpable.Of_stringable (M1)
end

include M

let of_basic ?(pointer : bool = false) (ty : Basic.t) : t =
  (if pointer then pointer_to else normal) ty

let basic_type : t -> Basic.t = function Normal x | Pointer_to x -> x

let basic_type_is (ty : t) ~(basic : Basic.t) : bool =
  Basic.equal (basic_type ty) basic

let is_pointer : t -> bool = function
  | Normal _ ->
      false
  | Pointer_to _ ->
      true

let deref : t -> t Or_error.t = function
  | Pointer_to k ->
      Or_error.return (Normal k)
  | Normal basic_type ->
    Or_error.error_s
      [%message "tried to get value type of a non-pointer type"
        (basic_type: Basic.t)
      ]

let ref : t -> t Or_error.t = function
  | Normal k ->
      Or_error.return (Pointer_to k)
  | Pointer_to _ ->
      Or_error.error_string "already a pointer type"

let is_atomic (ty : t) : bool = Basic.is_atomic (basic_type ty)

(* for now *)

let strip_atomic : t -> t = function
  | Normal k ->
      k |> Basic.strip_atomic |> normal
  | Pointer_to k ->
      k |> Basic.strip_atomic |> pointer_to

let to_non_atomic : t -> t Or_error.t = function
  | Normal k ->
      Or_error.(k |> Basic.to_non_atomic >>| normal)
  | Pointer_to k ->
      Or_error.(k |> Basic.to_non_atomic >>| pointer_to)

let bool ?(atomic : bool option) ?(pointer : bool option) () : t =
  of_basic (Basic.bool ?atomic ()) ?pointer

let int ?(atomic : bool option) ?(pointer : bool option) () : t =
  of_basic (Basic.int ?atomic ()) ?pointer

module Json : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Of_stringable (M1)

include Json

let check (t1 : t) (t2 : t) : unit Or_error.t =
  if equal t1 t2 then Result.ok_unit
  else Or_error.error_s [%message "Type mismatch" ~t1:(t1 : t) ~t2:(t2 : t)]
