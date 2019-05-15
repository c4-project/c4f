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
open Travesty_core_kernel_exts
open Act_utils
include Mini_env_intf

module Make (E : Basic) : S = struct
  let env = E.env

  module Random_var : sig
    type t = C_identifier.t [@@deriving sexp_of]

    include Quickcheck.S with type t := t
  end = struct
    type t = C_identifier.t

    let sexp_of_t = C_identifier.sexp_of_t

    let quickcheck_generator : C_identifier.t Quickcheck.Generator.t =
      (* We use a thunk here to prevent the generator from immediately
         raising an error if we try to create an empty environment. *)
      Quickcheck.Generator.of_fun (fun () ->
          match C_identifier.Map.keys E.env with
          | [] ->
              Error.raise_s
                [%message
                  "Tried to get a random variable from an empty environment"
                    ~here:[%here]]
          | xs ->
              Quickcheck.Generator.of_list xs )

    (* It's not clear whether we need a different observer here? *)
    let quickcheck_observer = C_identifier.quickcheck_observer

    (* Don't reduce identifiers, as this might make them no longer members
       of the environment. *)
    let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
  end

  let has_atomic_int_variables () : bool =
    C_identifier.Map.exists E.env
      ~f:Mini_type.(basic_type_is ~basic:Basic.atomic_int)

  let atomic_int_variables () : Mini_type.t C_identifier.Map.t =
    C_identifier.Map.filter E.env
      ~f:Mini_type.(basic_type_is ~basic:Basic.atomic_int)

  let has_int_variables () : bool =
    C_identifier.Map.exists E.env
      ~f:Mini_type.(basic_type_is ~basic:Basic.int)

  let int_variables () : Mini_type.t C_identifier.Map.t =
    C_identifier.Map.filter E.env
      ~f:Mini_type.(basic_type_is ~basic:Basic.int)
end

let test_env : Mini_type.t C_identifier.Map.t Lazy.t =
  lazy
    C_identifier.(
      Map.of_alist_exn
        Mini_type.
          [ (of_string "foo", normal Basic.int)
          ; (of_string "bar", pointer_to Basic.atomic_int)
          ; (of_string "barbaz", normal Basic.bool)
          ; (of_string "x", normal Basic.atomic_int)
          ; (of_string "y", normal Basic.atomic_int)
          ; (of_string "blep", pointer_to Basic.int) ])

let lift_to_lazy_mod (e : Mini_type.t C_identifier.Map.t Lazy.t) :
    (module S) Lazy.t =
  Lazy.(
    e
    >>| fun env ->
    ( module Make (struct
      let env = env
    end)
    : S ))

let test_env_mod : (module S) Lazy.t = lift_to_lazy_mod test_env

let test_env_atomic_ptrs_only : Mini_type.t C_identifier.Map.t Lazy.t =
  Lazy.(
    test_env
    >>| C_identifier.Map.filter
          ~f:
            Mini_type.(
              Fn.(is_pointer &&& basic_type_is ~basic:Basic.atomic_int)))

let test_env_atomic_ptrs_only_mod : (module S) Lazy.t =
  lift_to_lazy_mod test_env_atomic_ptrs_only

let empty_env_mod = lift_to_lazy_mod (lazy C_identifier.Map.empty)
