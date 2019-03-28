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
include My_quickcheck_intf

let gen_string_initial
    ~(initial : char Quickcheck.Generator.t)
    ~(rest : char Quickcheck.Generator.t)
    : string Quickcheck.Generator.t
  =
  Quickcheck.Generator.(
    map
      ~f:(Tuple2.uncurry ( ^ ))
      (tuple2 (map ~f:String.of_char initial) (String.gen' rest)))
;;

module Small_non_negative_int : sig
  include module type of Int
  include S_with_sexp with type t := int
end = struct
  include Int

  let quickcheck_generator = Base_quickcheck.Generator.small_positive_or_zero_int
end
