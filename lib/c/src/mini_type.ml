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

open Core_kernel
module Ac = Act_common
module Au = Act_utils

module Basic = struct
  module M = struct
    type t = Int | Atomic_int | Bool [@@deriving variants, enum]

    let table = [(Int, "int"); (Atomic_int, "atomic_int"); (Bool, "bool")]
  end

  include M
  include Au.Enum.Extend_table (M)

  let to_spec : t -> [> Act_c_lang.Ast.Type_spec.t] = function
    | Int ->
        `Int
    | Bool ->
        `Defined_type (Ac.C_id.of_string "bool")
    | Atomic_int ->
        `Defined_type (Ac.C_id.of_string "atomic_int")

  let to_non_atomic : t -> t Or_error.t = function
    | Atomic_int ->
        Or_error.return Int
    | Bool | Int ->
        Or_error.error_string "already non-atomic"
end

module M = struct
  type t = Normal of Basic.t | Pointer_to of Basic.t
  [@@deriving sexp, variants, eq, compare, quickcheck]
end

include M

let of_basic (ty : Basic.t) ~(is_pointer : bool) : t =
  (if is_pointer then pointer_to else normal) ty

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

let is_atomic (ty : t) : bool = basic_type_is ty ~basic:Atomic_int

(* for now *)

let to_non_atomic : t -> t Or_error.t = function
  | Normal k ->
      Or_error.(k |> Basic.to_non_atomic >>| normal)
  | Pointer_to k ->
      Or_error.(k |> Basic.to_non_atomic >>| pointer_to)

let%test_unit "basic_type_is compatibility with basic_type" =
  Base_quickcheck.Test.run_exn
    (module M)
    ~f:
      ([%test_pred: t] ~here:[[%here]] (fun t ->
           basic_type_is t ~basic:(basic_type t) ))
