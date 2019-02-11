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

include Fuzzer_action_intf

module Kind = struct
  type t =
    | Make_global
    | Make_constant_store
end

module Table = struct
  module Weight = Validated.Make (struct
      type t = int [@@deriving sexp]
      let here = [%here]
      let validate : t Validate.check =
        Validate.booltest
          Int.is_non_negative
          ~if_false:"Weights must be non-negative"
    end)

  module Row = struct
    type t =
      { action : (module S)
      ; weight : Weight.t
      }
    ;;
  end

  type t = Row.t list
end

module Payload = struct
  type t =
    | Make_global of { is_atomic : bool; initial_value : int }
    | Make_constant_store of { new_value : int }

  module Quickcheck = struct
    (*
    let anonymise = function
    | Make_global { is_atomic; initial_value } -> `A ((is_atomic, initial_value))
    | Make_store  { new_value : int } -> `B new_value
    ;;
       *)

    let deanonymise = function
    | `A ((is_atomic, initial_value)) -> Make_global { is_atomic; initial_value }
    | `B new_value -> Make_constant_store { new_value }
    ;;

    module G = Quickcheck.Generator

    (** [gen_int32_as_int] generates an [int] whose domain is that of
       [int32].  This is useful for making sure that we don't generate
       integers that could overflow when running tests on 32-bit
       platforms.  *)
    let gen_int32_as_int : int G.t =
      G.map ~f:(fun x -> Option.value ~default:0 (Int.of_int32 x)) Int32.gen
    ;;

    let gen : t G.t =
      G.map ~f:deanonymise
        (
          Quickcheck.Generator.variant2
            (* Make_global *) (G.tuple2 G.bool gen_int32_as_int)
            (* Make_store  *) (gen_int32_as_int)
        )
  end
  include Quickcheck
end
