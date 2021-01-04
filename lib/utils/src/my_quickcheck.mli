(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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
    produces non-empty strings whose first character draws from [initial] and
    all other characters from [rest]. *)

(** Convenience module for small non-negative integer generation. *)
module Small_non_negative_int : sig
  include module type of Int

  include S_with_sexp with type t := int
end

val print_sample :
     ?test_count:int
  -> ?printer:('a -> unit)
  -> (module S_sample with type t = 'a)
  -> unit
(** [print_sample ?test_count ?printer m] runs the quickcheck generator in
    [m] a small number of times ([test_count] or, if not given, 20), sorts
    and de-duplicates the results, and prints them to stdout, using [printer]
    if given or an S-expression representation otherwise. *)
