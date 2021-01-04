(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

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

let print_sample (type a) ?(test_count : int = 20)
    ?(printer : (a -> unit) option) (module M : S_sample with type t = a) :
    unit =
  let print =
    Option.value printer ~default:(fun x -> Stdio.print_s [%sexp (x : M.t)])
  in
  Base_quickcheck.Test.with_sample_exn [%quickcheck.generator: M.t]
    ~config:{Base_quickcheck.Test.default_config with test_count}
    ~f:(fun sequence ->
      sequence |> Sequence.to_list
      |> List.dedup_and_sort ~compare:M.compare
      |> List.iter ~f:print)
