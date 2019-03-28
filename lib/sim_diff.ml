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

open Base

type t =
  | Oracle_undefined
  | Subject_undefined
  | Result of Sim_output.State.Set.Partial_order.t
[@@deriving sexp]

let order_operator : Sim_output.State.Set.Partial_order.t -> string = function
  | Equal -> "=="
  | Subset _ -> "<<"
  | Superset _ -> ">>"
  | No_order _ -> "<>"
;;

let to_string : t -> string = function
  | Subject_undefined -> "UNDEFINED (Subject)"
  | Oracle_undefined -> "UNDEFINED (Oracle)"
  | Result o -> Printf.sprintf "Oracle %s Subject" (order_operator o)
;;

let pp : t Fmt.t = Fmt.of_to_string to_string

let compare_states
    ~(oracle_states : Sim_output.State.t list)
    ~(subject_states : Sim_output.State.t list)
    : t
  =
  let result = Sim_output.State.Set.(Travesty.T_fn.on of_list partial_compare oracle_states subject_states) in
  Result result
;;

let map_subject_states
  (states : Sim_output.State.t list)
  ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
  ~(value_map : string -> string Or_error.t)
  : Sim_output.State.t list Or_error.t =
  states
  |> List.map ~f:(Sim_output.State.map ~location_map ~value_map)
  |> Or_error.combine_errors
;;

let run
    ~(oracle : Sim_output.t)
    ~(subject : Sim_output.t)
    ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t)
  : t Or_error.t
  =
  let open Or_error.Let_syntax in
  if Sim_output.is_undefined oracle
  then return Oracle_undefined
  else if Sim_output.is_undefined subject
  then return Subject_undefined
  else
    let oracle_states = Sim_output.states oracle in
    let raw_subject_states = Sim_output.states subject in
    let%map subject_states = map_subject_states raw_subject_states ~location_map ~value_map in
    compare_states ~oracle_states ~subject_states
;;
