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
module Au = Act_utils

module Prim = struct
  type t = Int | Bool [@@deriving variants, equal, enumerate]
end

module Basic = struct
  module M = struct
    type t = {atomic: bool; prim: Prim.t} [@@deriving equal, enumerate]

    let int ?(atomic : bool = false) () : t = {atomic; prim= Int}

    let bool ?(atomic : bool = false) () : t = {atomic; prim= Bool}

    let table: (t, string) List.Assoc.t =
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

  let to_non_atomic : t -> t Or_error.t = function
    | {atomic= true; prim} ->
        Or_error.return {atomic= false; prim}
    | _ ->
        Or_error.error_string "already non-atomic"

  let is_atomic ({atomic; _} : t) : bool = atomic
end

module M = struct
  type t = Normal of Basic.t | Pointer_to of Basic.t
  [@@deriving sexp, variants, equal, compare, quickcheck]
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
  | Normal _ ->
      Or_error.error_string "not a pointer type"

let ref : t -> t Or_error.t = function
  | Normal k ->
      Or_error.return (Pointer_to k)
  | Pointer_to _ ->
      Or_error.error_string "already a pointer type"

let is_atomic (ty : t) : bool = Basic.is_atomic (basic_type ty)

(* for now *)

let to_non_atomic : t -> t Or_error.t = function
  | Normal k ->
      Or_error.(k |> Basic.to_non_atomic >>| normal)
  | Pointer_to k ->
      Or_error.(k |> Basic.to_non_atomic >>| pointer_to)

let bool ?(atomic : bool option) ?(pointer : bool option) () : t =
  of_basic (Basic.bool ?atomic ()) ?pointer

let int ?(atomic : bool option) ?(pointer : bool option) () : t =
  of_basic (Basic.int ?atomic ()) ?pointer

module Str = struct
  type nonrec t = t

  let to_string : t -> string = function
    | Normal k ->
        Basic.to_string k
    | Pointer_to k ->
        "*" ^ Basic.to_string k

  let of_string (str : string) : t =
    if String.is_suffix str ~suffix:"*" then
      Pointer_to (Basic.of_string (String.drop_suffix str 1))
    else Normal (Basic.of_string str)
end

include (Str : Stringable.S with type t := t)

module Json : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Of_stringable (Str)

include Json

let%test_unit "basic_type_is compatibility with basic_type" =
  Base_quickcheck.Test.run_exn
    (module M)
    ~f:
      ([%test_pred: t] ~here:[[%here]] (fun t ->
           basic_type_is t ~basic:(basic_type t)))
