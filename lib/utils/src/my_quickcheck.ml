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

module type S_with_sexp = sig
  type t [@@deriving sexp_of, quickcheck]
end

module type S_sample = sig
  type t [@@deriving sexp, compare, quickcheck]
end

let gen_string_initial ~(initial : char Base_quickcheck.Generator.t)
    ~(rest : char Base_quickcheck.Generator.t) :
    string Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.Let_syntax.(
    let%map x = initial and y = Base_quickcheck.Generator.string_of rest in
    String.of_char x ^ y)

module Small_non_negative_int : sig
  include module type of Int

  include S_with_sexp with type t := int
end = struct
  include Int

  let quickcheck_generator =
    Base_quickcheck.Generator.small_positive_or_zero_int

  let quickcheck_shrinker = Base_quickcheck.Shrinker.int

  let quickcheck_observer = Base_quickcheck.Observer.int
end

let print_sample (type a) ?(printer : (a -> unit) option)
    (module M : S_sample with type t = a) : unit =
  let print =
    Option.value printer ~default:(fun x -> Stdio.print_s [%sexp (x : M.t)])
  in
  Base_quickcheck.Test.with_sample_exn [%quickcheck.generator: M.t]
    ~config:{Base_quickcheck.Test.default_config with test_count= 20}
    ~f:(fun sequence ->
      sequence |> Sequence.to_list
      |> List.dedup_and_sort ~compare:M.compare
      |> List.iter ~f:print)
