(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Tx = Travesty_base_exts

module Weight = struct
  include Validated.Make (struct
    type t = int [@@deriving sexp]

    let here = [%here]

    let validate : t Validate.check =
      Validate.booltest Int.is_non_negative
        ~if_false:"Weights must be non-negative"
  end)

  module Qc : Quickcheck.S with type t := t = struct
    module G = Quickcheck.Generator
    module O = Quickcheck.Observer
    module S = Quickcheck.Shrinker

    let quickcheck_generator : t G.t =
      G.map ~f:create_exn G.small_non_negative_int

    let quickcheck_observer : t O.t = O.unmap ~f:raw Int.quickcheck_observer

    let quickcheck_shrinker : t S.t =
      S.map ~f:create_exn ~f_inverse:raw Int.quickcheck_shrinker
  end

  include Qc
end

module Row = struct
  type 'a t = {item: 'a; weight: Weight.t} [@@deriving sexp_of, fields]

  let adjust_weight (row : 'a t) ~(f : 'a -> int -> int) : 'a t Or_error.t =
    let open Or_error.Let_syntax in
    let raw_weight' = f row.item (Weight.raw row.weight) in
    let%map weight' = Weight.create raw_weight' in
    {row with weight= weight'}

  module On_monad (M : sig
    include Monad.S

    val lift : 'a Or_error.t -> 'a t
  end) =
  struct
    let adjust_weight_m (row : 'a t) ~(f : 'a -> int -> int M.t) : 'a t M.t =
      let open M.Let_syntax in
      let%bind raw_weight' = f row.item (Weight.raw row.weight) in
      let%map weight' = M.lift (Weight.create raw_weight') in
      {row with weight= weight'}
  end

  module Qc : Quickcheck.S1 with type 'a t := 'a t = struct
    module G = Quickcheck.Generator
    module O = Quickcheck.Observer
    module S = Quickcheck.Shrinker

    let to_tuple {item; weight} = (item, weight)

    let of_tuple (item, weight) = {item; weight}

    let quickcheck_generator (g : 'a G.t) : 'a t G.t =
      G.map ~f:of_tuple (G.tuple2 g Weight.quickcheck_generator)

    let quickcheck_observer (o : 'a O.t) : 'a t O.t =
      O.unmap ~f:to_tuple (O.tuple2 o Weight.quickcheck_observer)

    let quickcheck_shrinker (s : 'a S.t) : 'a t S.t =
      S.map
        (S.tuple2 s Weight.quickcheck_shrinker)
        ~f:of_tuple ~f_inverse:to_tuple
  end

  include Qc
end

type 'a t = 'a Row.t list [@@deriving sexp_of]

let from_alist_row ((item, raw_weight) : 'a * int) : 'a Row.t Or_error.t =
  let open Or_error.Let_syntax in
  let%map weight = Weight.create raw_weight in
  {Row.item; weight}

let from_alist : ('a, int) List.Assoc.t -> 'a t Or_error.t = function
  | [] ->
      Or_error.error_string "Weighted lists cannot be empty"
  | alist ->
      alist |> List.map ~f:from_alist_row |> Or_error.combine_errors

let from_alist_exn (alist : ('a, int) List.Assoc.t) : 'a t =
  Or_error.ok_exn (from_alist alist)

let adjust_weights (wl : 'a t) ~(f : 'a -> int -> int) : 'a t Or_error.t =
  wl |> List.map ~f:(Row.adjust_weight ~f) |> Or_error.combine_errors

let adjust_weights_exn (wl : 'a t) ~(f : 'a -> int -> int) : 'a t =
  Or_error.ok_exn (adjust_weights wl ~f)

module On_monad (M : sig
  include Monad.S

  val lift : 'a Or_error.t -> 'a t
end) =
struct
  module L = Tx.List.On_monad (M)
  module R = Row.On_monad (M)

  let adjust_weights_m (wl : 'a t) ~(f : 'a -> int -> int M.t) : 'a t M.t =
    L.map_m wl ~f:(R.adjust_weight_m ~f)
end

module Cumulative = struct
  type 'a w = 'a t

  type 'a t = {rows: 'a Row.t list; max: int [@sexp.opaque]}
  [@@deriving sexp_of]

  let from_table_step (total : int) (row : 'a Row.t) : int * 'a Row.t option
      =
    match Weight.raw row.Row.weight with
    | 0 ->
        (total, None)
    | this_weight ->
        let total' = total + this_weight in
        let row' =
          {Row.weight= Weight.create_exn total'; item= row.Row.item}
        in
        (total', Some row')

  let of_weighted_list (wl : 'a w) : 'a t Or_error.t =
    let max, opt_rows = List.fold_map wl ~init:0 ~f:from_table_step in
    match List.filter_opt opt_rows with
    | [] ->
        Or_error.error_string "No choices have a positive weight"
    | rows ->
        Or_error.return {max; rows}

  let get (cl : 'a t) (position : int) : 'a =
    let possible =
      List.drop_while cl.rows ~f:(fun {weight; _} ->
          Weight.raw weight < position )
    in
    Row.item (List.hd_exn possible)

  let sample (cl : 'a t) ~(random : Splittable_random.State.t) : 'a =
    let position = Splittable_random.int random ~lo:0 ~hi:cl.max in
    get cl position
end

let sample (wl : 'a t) ~(random : Splittable_random.State.t) : 'a Or_error.t
    =
  Or_error.(wl |> Cumulative.of_weighted_list >>| Cumulative.sample ~random)

let sample_exn (wl : 'a t) ~(random : Splittable_random.State.t) : 'a =
  Or_error.ok_exn (sample wl ~random)

let sample_gen_exn (wl : 'a t) : 'a Quickcheck.Generator.t =
  Quickcheck.Generator.create (fun ~size -> ignore size ; sample_exn wl)

let fold (wl : 'a t) ~(f : 'b -> 'a -> int -> 'b) : init:'b -> 'b =
  List.fold wl ~f:(fun acc {item; weight} -> f acc item (Weight.raw weight))

let iter (wl : 'a t) ~(f : 'a -> int -> unit) : unit =
  fold wl ~f:(Fn.const f) ~init:()

module Qc : Quickcheckable.S1 with type 'a t := 'a t = struct
  module G = Quickcheck.Generator
  module O = Quickcheck.Observer
  module S = Quickcheck.Shrinker

  let quickcheck_generator (g : 'a G.t) : 'a t G.t =
    List.quickcheck_generator (Row.quickcheck_generator g)

  let quickcheck_observer (o : 'a O.t) : 'a t O.t =
    List.quickcheck_observer (Row.quickcheck_observer o)

  (* TODO(@MattWindsor91): add a shrinker here, but make sure it can't shrink
     to empty lists. *)
  let quickcheck_shrinker (_ : 'a S.t) : 'a t S.t = S.empty ()
end

include Qc
