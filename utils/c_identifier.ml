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

module M = Validated.Make_bin_io_compare_hash_sexp (struct
    include String
    let here = [%here]

    let validate_initial_char : char Validate.check =
      Validate.booltest
        (Travesty.T_fn.disj Char.is_alpha (Char.equal '_'))
        ~if_false:"Invalid initial character."
    ;;

    let validate_char : char Validate.check =
      Validate.booltest
        (Travesty.T_fn.disj Char.is_alphanum (Char.equal '_'))
        ~if_false:"Invalid character."
    ;;

    let validate_sep : (char * char list) Validate.check =
      Validate.pair
        ~fst:(
          fun c ->
            Validate.name (sprintf "char '%c'" c)
              (validate_initial_char c)
        )
        ~snd:(
            Validate.list ~name:(sprintf "char '%c'")
              validate_char
        )
    ;;

    let validate : t Validate.check =
      fun id ->
        match String.to_list id with
        | [] -> Validate.fail_s
                  [%message "Identifiers can't be empty"
                      ~id]
        | c :: cs -> validate_sep (c, cs)
    ;;

    let validate_binio_deserialization = true
  end)
include M
include Comparable.Make (M)

let to_string : t -> string = raw
let of_string : string -> t = create_exn

let pp : t Fmt.t = Fmt.of_to_string to_string

module Q : Quickcheck.S with type t := t = struct
  let char_or_underscore (c : char Quickcheck.Generator.t)
      : char Quickcheck.Generator.t =
    Quickcheck.Generator.(union [ c; return '_' ])
  ;;

  let gen_initial : string Quickcheck.Generator.t =
    Quickcheck.Generator.(
      map ~f:String.of_char (char_or_underscore Char.gen_alpha)
    )
  ;;

  let gen_rest : string Quickcheck.Generator.t =
    String.gen' (char_or_underscore Char.gen_alphanum)
  ;;

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.(
      map ~f:(Fn.compose create_exn (Tuple2.uncurry (^)))
        (tuple2 gen_initial gen_rest)
    )
  ;;

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.create
      (fun ident ->
         ident
         |> raw
         |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
         |> Sequence.filter_map
           ~f:(fun x -> x |> create |> Result.ok)
      )
  ;;
end
include Q
