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

include Mini_env_intf

module Make (E : Basic) : S = struct
  let env = E.env

  let random_var : C_identifier.t Quickcheck.Generator.t =
    Quickcheck.Generator.of_list (C_identifier.Map.keys E.env)

  let atomic_int_variables () : Mini_type.t C_identifier.Map.t =
    C_identifier.Map.filter E.env
      ~f:(Mini_type.(basic_type_is ~basic:Basic.atomic_int))
  ;;

  let int_variables () : Mini_type.t C_identifier.Map.t =
    C_identifier.Map.filter E.env
      ~f:(Mini_type.(basic_type_is ~basic:Basic.int))
  ;;
end

(** An environment used for testing the various environment-sensitive
    operations. *)
let test_env : Mini_type.t C_identifier.Map.t Lazy.t =
  lazy
    C_identifier.(
      Map.of_alist_exn
        Mini_type.
          [ of_string "foo"   , normal     Basic.int
          ; of_string "bar"   , pointer_to Basic.atomic_int
          ; of_string "barbaz", normal     Basic.bool
          ; of_string "x"     , normal     Basic.atomic_int
          ; of_string "y"     , normal     Basic.atomic_int
          ; of_string "blep"  , pointer_to Basic.int
          ]
    )

let test_env_mod : (module S) Lazy.t =
  Lazy.(
    test_env
    >>| fun env -> (module (Make (struct let env = env end)) : S)
  )
;;
