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
open Utils

module M = struct
  type t = string Litmus.Id.Map.t [@@deriving sexp, compare]
end

include M

module Q : My_quickcheck.S_with_sexp with type t := t = struct
  include M

  let quickcheck_generator =
    Litmus.Id.Map.quickcheck_generator [%quickcheck.generator: Litmus.Id.t]
      Base_quickcheck.quickcheck_generator_string

  let quickcheck_observer =
    Litmus.Id.Map.quickcheck_observer [%quickcheck.observer: Litmus.Id.t]
      Base_quickcheck.quickcheck_observer_string

  let quickcheck_shrinker =
    Litmus.Id.Map.quickcheck_shrinker [%quickcheck.shrinker: Litmus.Id.t]
      Base_quickcheck.quickcheck_shrinker_string
end

include Q

let of_alist = Litmus.Id.Map.of_alist_or_error

let bound = Litmus.Id.Map.keys

let restrict (state : t) ~(domain : Litmus.Id.Set.t) : t =
  Litmus.Id.Map.filter_keys state ~f:(Litmus.Id.Set.mem domain)

let map ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t) (state : t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let f (k, v) =
    let%bind ko = location_map k in
    let%map v' = value_map v in
    let open Option.Let_syntax in
    let%map k' = ko in
    (k', v')
  in
  let alist = Litmus.Id.Map.to_alist state in
  (* First, fail the map if there were any errors... *)
  let%bind mapped = Or_error.combine_errors (List.map ~f alist) in
  (* Next, remove any items with no key mapping... *)
  let alist' = List.filter_opt mapped in
  Litmus.Id.Map.of_alist_or_error alist'

module Set = My_set.Extend (Set.Make (M))
