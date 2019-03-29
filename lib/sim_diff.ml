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
  let result =
    Sim_output.State.Set.(
      Travesty.T_fn.on of_list partial_compare oracle_states subject_states)
  in
  Result result
;;

let map_subject_states
    (states : Sim_output.State.t list)
    ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t)
    : Sim_output.State.t list Or_error.t
  =
  states
  |> List.map ~f:(Sim_output.State.map ~location_map ~value_map)
  |> Or_error.combine_errors
;;

let check_domain_consistency
    (xs_domains : Litmus.Id.Set.t Sequence.t)
    (x_domain : Litmus.Id.Set.t)
    : unit Or_error.t
  =
  let inconsistencies : unit Or_error.t Sequence.t =
    xs_domains
    |> Sequence.filter ~f:(Fn.non ([%equal: Litmus.Id.Set.t] x_domain))
    |> Sequence.map ~f:(fun bad_dom ->
           Or_error.error_s
             [%message
               "Domains of states are inconsistent: for example,"
                 ~here:[%here]
                 ~one_domain:(x_domain : Litmus.Id.Set.t)
                 ~other_domain:(bad_dom : Litmus.Id.Set.t)])
  in
  Or_error.combine_errors_unit (Sequence.to_list inconsistencies)
;;

let%test_module "check_domain_consistency expects tests" =
  (module struct
    let test_x_domain =
      Litmus.Id.(Set.of_list [ of_string "0:foo"; of_string "1:bar"; of_string "baz" ])
    ;;

    let%expect_test "no other domains" =
      Stdio.print_s
        [%sexp (check_domain_consistency Sequence.empty test_x_domain : unit Or_error.t)];
      [%expect {| (Ok ()) |}]
    ;;

    let%expect_test "only consistent domains" =
      let doms = Sequence.of_list (List.init 10 ~f:(Fn.const test_x_domain)) in
      Stdio.print_s
        [%sexp (check_domain_consistency doms test_x_domain : unit Or_error.t)];
      [%expect {| (Ok ()) |}]
    ;;

    let%expect_test "inconsistent domains" =
      let doms = Sequence.singleton Litmus.Id.(Set.of_list [ of_string "0:foo" ]) in
      Stdio.print_s
        [%sexp (check_domain_consistency doms test_x_domain : unit Or_error.t)];
      [%expect
        {|
      (Error
       ("Domains of states are inconsistent: for example,"
        (here lib/sim_diff.ml:83:23) (one_domain (0:foo 1:bar baz))
        (other_domain (0:foo)))) |}]
    ;;
  end)
;;

let get_domain : Sim_output.State.t list -> Litmus.Id.Set.t Or_error.t = function
  | [] -> Or_error.return Litmus.Id.Set.empty
  | x :: xs ->
    let dom s = Litmus.Id.Set.of_list (Sim_output.State.bound s) in
    let x_domain = dom x in
    let xs_domains = Sequence.map ~f:dom (Sequence.of_list xs) in
    x_domain |> Travesty.T_or_error.tee_m ~f:(check_domain_consistency xs_domains)
;;

let filter_oracle_states
    ~(raw_oracle_states : Sim_output.State.t list)
    ~(subject_states : Sim_output.State.t list)
    : Sim_output.State.t list Or_error.t
  =
  let open Or_error.Let_syntax in
  let%map domain = get_domain subject_states in
  List.map raw_oracle_states ~f:(Sim_output.State.restrict ~domain)
;;

let run_defined
    ~(oracle : Sim_output.t)
    ~(subject : Sim_output.t)
    ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t)
    : t Or_error.t
  =
  let open Or_error.Let_syntax in
  let raw_oracle_states = Sim_output.states oracle in
  let raw_subject_states = Sim_output.states subject in
  let%bind subject_states =
    map_subject_states raw_subject_states ~location_map ~value_map
  in
  let%map oracle_states = filter_oracle_states ~raw_oracle_states ~subject_states in
  compare_states ~oracle_states ~subject_states
;;

let run
    ~(oracle : Sim_output.t)
    ~(subject : Sim_output.t)
    ~(location_map : Litmus.Id.t -> Litmus.Id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t)
    : t Or_error.t
  =
  if Sim_output.is_undefined oracle
  then Or_error.return Oracle_undefined
  else if Sim_output.is_undefined subject
  then Or_error.return Subject_undefined
  else run_defined ~oracle ~subject ~location_map ~value_map
;;
