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

(** Miscellaneous utilities for the Jane Street quickcheck system. *)

(** Signature of modules that expose a type with both sexp-of and quickcheck
    functionality. *)
module type S_with_sexp = sig
  type t [@@deriving sexp_of, quickcheck]
end

(** Signature of modules that, in addition, have comparison, and therefore
    can be checked for unique sampling. *)
module type S_sample = sig
  type t [@@deriving sexp, compare, quickcheck]
end

val gen_string_initial :
     initial:char Base_quickcheck.Generator.t
  -> rest:char Base_quickcheck.Generator.t
  -> string Base_quickcheck.Generator.t
(** [gen_string_initial ~initial ~rest] is a Quickcheck generator that
    produces non-empty strings whose first character draws from [initial]
    and all other characters from [rest]. *)

(** Convenience module for small non-negative integer generation. *)
module Small_non_negative_int : sig
  include module type of Int

  include S_with_sexp with type t := int
end

val print_sample : (module S_sample) -> unit
(** [print_sample m] runs the quickcheck generator in [m] a small number of
    times, sorts and de-duplicates the results, and prints them as
    S-expressions to stdout. *)
