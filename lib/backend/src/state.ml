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
open Act_utils

module A = Act_common
module Tx = Travesty_base_exts

module M = struct
  type t = string Map.M(A.Litmus_id).t [@@deriving sexp, compare]
end

include M
include Comparable.Make (M)

module J : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Make_map (A.Litmus_id) (Plumbing.Jsonable.String)

include J

module Q : My_quickcheck.S_with_sexp with type t := t = struct
  include M
  open Base_quickcheck

  let quickcheck_generator : t Generator.t =
    Generator.map_t_m
      (module A.Litmus_id)
      [%quickcheck.generator: A.Litmus_id.t]
      Base_quickcheck.quickcheck_generator_string

  let quickcheck_observer : t Observer.t =
    Observer.map_t [%quickcheck.observer: A.Litmus_id.t]
      Base_quickcheck.quickcheck_observer_string

  let quickcheck_shrinker : t Shrinker.t =
    Shrinker.map_t [%quickcheck.shrinker: A.Litmus_id.t]
      Base_quickcheck.quickcheck_shrinker_string
end

include Q

let of_alist = Map.of_alist_or_error (module A.Litmus_id)

let domain : t -> Set.M(A.Litmus_id).t = Fn.compose (Set.of_list (module A.Litmus_id)) Map.keys


let domain_error (one_domain : Set.M(A.Litmus_id).t)
    (another_domain : Set.M(A.Litmus_id).t) : unit Or_error.t =
  Or_error.error_s
    [%message
      "Domains of states are inconsistent: for example,"
        ~one_domain:(one_domain : Set.M(A.Litmus_id).t)
        ~another_domain:(another_domain : Set.M(A.Litmus_id).t)]

let check_domain_consistency (xs_domains : Set.M(A.Litmus_id).t Sequence.t)
    (x_domain : Set.M(A.Litmus_id).t) : unit Or_error.t =
  let inconsistencies : unit Or_error.t Sequence.t =
    xs_domains
    |> Sequence.filter ~f:(Fn.non (Set.equal x_domain))
    |> Sequence.map ~f:(domain_error x_domain)
  in
  Or_error.combine_errors_unit (Sequence.to_list inconsistencies)

let common_domain : t list -> Set.M(A.Litmus_id).t Or_error.t = function
  | [] ->
      Or_error.return (Set.empty (module A.Litmus_id))
  | x :: xs ->
      let xs_domains = Sequence.map ~f:domain (Sequence.of_list xs) in
      Tx.Or_error.tee_m (domain x) ~f:(check_domain_consistency xs_domains)

let restrict (state : t) ~(domain : Set.M(A.Litmus_id).t) : t =
  Map.filter_keys state ~f:(Set.mem domain)

let map ~(location_map : A.Litmus_id.t -> A.Litmus_id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t) (state : t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let f (k, v) =
    let%bind ko = location_map k in
    let%map v' = value_map v in
    let open Option.Let_syntax in
    let%map k' = ko in
    (k', v')
  in
  let alist = Map.to_alist state in
  (* First, fail the map if there were any errors... *)
  let%bind mapped = Or_error.combine_errors (List.map ~f alist) in
  (* Next, remove any items with no key mapping... *)
  let alist' = List.filter_opt mapped in
  Map.of_alist_or_error (module A.Litmus_id) alist'
