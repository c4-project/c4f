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

module type Basic = sig
  val here : Lexing.position

  val validate_initial_char : char Validate.check

  val validate_char : char Validate.check
end

module Make (B : Basic) = struct
  include String

  let here = B.here

  let validate_sep : (char * char list) Validate.check =
    Validate.pair
      ~fst:(fun c ->
        Validate.name (sprintf "char '%c'" c) (B.validate_initial_char c) )
      ~snd:(Validate.list ~name:(sprintf "char '%c'") B.validate_char)

  let validate : t Validate.check =
   fun id ->
    match String.to_list id with
    | [] ->
        Validate.fail_s [%message "Identifiers can't be empty" ~id]
    | c :: cs ->
        validate_sep (c, cs)

  let validate_binio_deserialization = true
end

module M = Validated.Make_bin_io_compare_hash_sexp (Make (struct
  let here = [%here]

  let validate_initial_char : char Validate.check =
    Validate.booltest
      (Travesty.T_fn.disj Char.is_alpha (Char.equal '_'))
      ~if_false:"Invalid initial character."

  let validate_char : char Validate.check =
    Validate.booltest
      (Travesty.T_fn.disj Char.is_alphanum (Char.equal '_'))
      ~if_false:"Invalid character."
end))

include M
include Comparable.Make (M)

let to_string : t -> string = raw

let of_string : string -> t = create_exn

let pp : t Fmt.t = Fmt.of_to_string to_string

let is_string_safe (str : string) : bool = Or_error.is_ok (create str)

let%expect_test "is_string_safe: positive example" =
  Stdio.printf "%b\n" (is_string_safe "t0r0") ;
  [%expect {| true |}]

let%expect_test "is_string_safe: positive (but not herd-safe) example" =
  Stdio.printf "%b\n" (is_string_safe "_t0r0") ;
  [%expect {| true |}]

let%expect_test "is_string_safe: negative example" =
  Stdio.printf "%b\n" (is_string_safe "0r0") ;
  [%expect {| false |}]

module Q : Quickcheck.S with type t := t = struct
  let char_or_underscore (c : char Quickcheck.Generator.t) :
      char Quickcheck.Generator.t =
    Quickcheck.Generator.(union [c; return '_'])

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (My_quickcheck.gen_string_initial
         ~initial:(char_or_underscore Char.gen_alpha)
         ~rest:(char_or_underscore Char.gen_alphanum))
      ~f:create_exn

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.create (fun ident ->
        ident |> raw
        |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
        |> Sequence.filter_map ~f:(Fn.compose Result.ok create) )
end

include Q

module Herd_safe = struct
  type c = t

  module M = Validated.Make_bin_io_compare_hash_sexp (Make (struct
    let here = [%here]

    let validate_initial_char : char Validate.check =
      Validate.booltest Char.is_alpha
        ~if_false:"Invalid initial character (must be alphabetic)."

    let validate_char : char Validate.check =
      Validate.booltest Char.is_alphanum
        ~if_false:"Invalid non-initial character (must be alphanumeric)."
  end))

  include M
  include Comparable.Make (M)

  let of_c_identifier (cid : c) : t Or_error.t = cid |> to_string |> create

  let to_c_identifier (hid : t) : c = hid |> raw |> of_string

  let is_string_safe (str : string) : bool = Or_error.is_ok (create str)

  let%expect_test "is_string_safe: positive example" =
    Stdio.printf "%b\n" (is_string_safe "t0r0") ;
    [%expect {| true |}]

  let%expect_test "is_string_safe: negative example" =
    Stdio.printf "%b\n" (is_string_safe "_t0r0") ;
    [%expect {| false |}]

  let to_string : t -> string = raw

  let of_string : string -> t = create_exn

  let pp : t Fmt.t = Fmt.of_to_string to_string

  module Q : Quickcheck.S with type t := t = struct
    let quickcheck_generator : t Quickcheck.Generator.t =
      Quickcheck.Generator.map
        (My_quickcheck.gen_string_initial ~initial:Char.gen_alpha
           ~rest:Char.gen_alphanum)
        ~f:create_exn

    let quickcheck_observer : t Quickcheck.Observer.t =
      Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

    let quickcheck_shrinker : t Quickcheck.Shrinker.t =
      Quickcheck.Shrinker.create (fun ident ->
          ident |> raw
          |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
          |> Sequence.filter_map ~f:(Fn.compose Result.ok create) )
  end

  include Q
end
