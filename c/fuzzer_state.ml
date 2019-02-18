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

type t =
  { vars : Fuzzer_var.Map.t
  }
[@@deriving fields]
;;

let init
    (globals : Mini.Type.t C_identifier.Map.t)
    (locals  : C_identifier.Set.t)
  : t =
  let vars =
    Fuzzer_var.Map.make_existing_var_map globals locals in
  { vars }
;;

let try_map_vars
    (s : t)
    ~(f : Fuzzer_var.Map.t -> Fuzzer_var.Map.t Or_error.t) : t Or_error.t =
  Or_error.(s.vars |> f >>| fun vars -> { vars })
;;

let map_vars (s : t) ~(f : Fuzzer_var.Map.t -> Fuzzer_var.Map.t) : t =
  { vars = f s.vars }
;;

let register_global
    ?(initial_value : Fuzzer_var.Value.t option)
    (s : t)
    (var : C_identifier.t) (ty : Mini.Type.t) : t =
  map_vars s
    ~f:(
      fun v ->
        Fuzzer_var.Map.register_global v ?initial_value var ty
    )
;;

let erase_var_value
    (s : t) ~(var : C_identifier.t) : t Or_error.t =
  try_map_vars s ~f:(Fuzzer_var.Map.erase_value ~var)

module Monad = struct
  include Travesty.State_transform.Make (struct
      module Inner = Or_error
      type nonrec t = t
    end)
  ;;

  let with_vars_m (f : Fuzzer_var.Map.t -> 'a t) : 'a t =
    peek vars >>= f
  ;;

  let with_vars (f : Fuzzer_var.Map.t -> 'a) : 'a t =
    peek vars >>| f
  ;;

  let register_global
      ?(initial_value : Fuzzer_var.Value.t option)
      (ty : Mini.Type.t)
      (var : C_identifier.t)
    : unit t =
    modify (fun s -> register_global ?initial_value s var ty)

  let erase_var_value (var : C_identifier.t) : unit t =
    Monadic.modify (erase_var_value ~var)
end
