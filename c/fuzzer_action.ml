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
      { action : (module S) sexp_opaque
      ; weight : Weight.t
      }
      [@@deriving sexp, fields]
    ;;

    let some_if_available
        (row : t)
        ~(subject : Fuzzer_subject.Test.t)
      : t option Fuzzer_state.Monad.t =
      let open Fuzzer_state.Monad.Let_syntax in
      let (module A) = row.action in
      let%map is_available = A.available subject in
      let is_weight_nonzero = Int.(0 < (Weight.raw row.weight)) in
      Option.some_if (is_available && is_weight_nonzero) row
    ;;

    let compare_weights : t -> t -> int =
      Travesty.T_fn.on
        (Fn.compose Weight.raw weight)
        Int.compare
    ;;
  end

  type t = Row.t list

  module Cumulative = struct
    include Validated.Make (struct
        type t = Row.t list [@@deriving sexp]

        let here = [%here]
        let validate : t Validate.check =
          Validate.booltest
            (List.is_sorted ~compare:Row.compare_weights)
            ~if_false:"Weights aren't cumulative"
      end)

    let from_table_inner : Row.t list -> (int * Row.t list) =
      Travesty.T_list.fold_map
        ~init:0
        ~f:(fun total row ->
            let this_weight = Weight.raw row.Row.weight in
            let total' = total + this_weight in
            total', { Row.weight = Weight.create_exn total'
                    ; action = row.Row.action
                    }
          )
    ;;

    let from_table (table : Row.t list) : int * t =
      let (max, raw) = from_table_inner table in
      max, create_exn raw
    ;;

    let get (c_table : t) (position : int) : (module S) option =
      let possible =
        List.drop_while (raw c_table)
          ~f:(fun { weight; _ } -> Weight.raw weight < position)
      in
      Option.map ~f:(Row.action) (List.hd possible)
    ;;

  end

  module State_list = Travesty.T_list.On_monad(Fuzzer_state.Monad)

  (** [to_available_only table subject] is a stateful action that
     filters [table] to contain only those rows whose actions are
     available on [subject]. *)
  let to_available_only (table : t) (subject : Fuzzer_subject.Test.t)
    : t Fuzzer_state.Monad.t =
    let open Fuzzer_state.Monad.Let_syntax in
    let%map options =
      State_list.map_m ~f:(Row.some_if_available ~subject) table
    in
    List.filter_opt options
  ;;


  let pick_from_cumulative_table
      (max : int) (c_table : Cumulative.t)
      (rng : Splittable_random.State.t)
    : (module S) Fuzzer_state.Monad.t =
    let position = Splittable_random.int rng ~lo:0 ~hi:(max - 1) in
    Cumulative.get c_table position
    |> Result.of_option
      ~error:(
        Error.create_s
          [%message
            "Couldn't pick action from cumulative table: is the table empty?"
              ~here:[%here]
              ~table:(c_table : Cumulative.t)
              ~position:(position : int)
              ~max:(max : int)
          ]
      )
    |> Fuzzer_state.Monad.Monadic.return
  ;;

  (** [pick table subject] is a stateful action that picks a
     weighted-random action module from [table] that is available on
     [subject]. *)
  let pick (table : t) (subject : Fuzzer_subject.Test.t)
    : (module S) Fuzzer_state.Monad.t =
    let open Fuzzer_state.Monad.Let_syntax in
    let%bind available = to_available_only table subject in
    let (max, c_table) = Cumulative.from_table available in
    Fuzzer_state.Monad.with_rng_m
      (pick_from_cumulative_table max c_table)
  ;;
end
