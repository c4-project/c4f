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

open Core_kernel
open Utils

module Basic = struct
  module M = struct
    type t =
      | Int
      | Atomic_int
      | Bool
    [@@deriving variants, enum]
    ;;

    let table =
      [ Int       , "int"
      ; Atomic_int, "atomic_int"
      ; Bool      , "bool"
      ]
    ;;
  end

  include M
  include Enum.Extend_table (M)

  let to_spec : t -> [> Ast.Type_spec.t] = function
    | Int -> `Int
    | Bool -> `Defined_type (C_identifier.of_string "bool")
    | Atomic_int -> `Defined_type (C_identifier.of_string "atomic_int")
  ;;
end

type t =
  | Normal of Basic.t
  | Pointer_to of Basic.t
[@@deriving sexp, variants, eq, compare]
;;

let of_basic (ty : Basic.t) ~(is_pointer : bool) : t =
  (if is_pointer then pointer_to else normal) ty
;;

let underlying_basic_type : t -> Basic.t = function
  | Normal x | Pointer_to x -> x
;;

let is_pointer : t -> bool = function
  | Normal     _ -> false
  | Pointer_to _ -> true
;;

let deref : t -> t Or_error.t = function
  | Pointer_to k -> Or_error.return (Normal k)
  | Normal _ -> Or_error.error_string "not a pointer type"
;;

let ref : t -> t Or_error.t = function
  | Normal k -> Or_error.return (Pointer_to k)
  | Pointer_to _ -> Or_error.error_string "already a pointer type"
;;

let is_atomic (ty : t) : bool =
  Basic.equal Atomic_int (underlying_basic_type ty)
;;

module Quickcheck_main : Quickcheckable.S with type t := t = struct
  module G = Core_kernel.Quickcheck.Generator
  module O = Core_kernel.Quickcheck.Observer
  module S = Core_kernel.Quickcheck.Shrinker

  (** Converts the type variant to its anonymous quickcheck form. *)
  let anonymise = function
    | Normal     b -> `A b
    | Pointer_to b -> `B b
  ;;

  (** Converts the type variant from its anonymous quickcheck form. *)
  let deanonymise = function
    | `A b -> Normal b
    | `B b -> Pointer_to b
  ;;

  let gen      : t G.t =
    G.map (G.variant2 Basic.gen Basic.gen) ~f:deanonymise
  let obs      : t O.t =
    O.unmap (O.variant2 Basic.obs Basic.obs) ~f:anonymise
  let shrinker : t S.t =
    S.map (S.variant2 Basic.shrinker Basic.shrinker)
      ~f:deanonymise ~f_inverse:anonymise
  ;;
end
include Quickcheck_main

