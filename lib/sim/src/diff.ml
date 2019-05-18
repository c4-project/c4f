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
module A = Act_common
module Au = Act_utils
module Tx = Travesty_base_exts
module Ob = Output.Observation

module Order = struct
  include Au.Set_partial_order.M(State)

  let operator : Ordering.t option -> string = function
    | Some Equal ->
        "=="
    | Some Less ->
        "<<"
    | Some Greater ->
        ">>"
    | None ->
        "<>"

  let colour : Ordering.t option -> Fmt.style = function
    | Some Equal ->
        `Green
    | Some Less ->
        `Red
    | Some Greater ->
        `Yellow
    | None ->
        `Magenta

  let pp_operator (f : Formatter.t) (op : Ordering.t option) : unit =
    Fmt.(styled (colour op) (using operator string)) f op
end

type t = Oracle_undefined | Subject_undefined | Result of Order.t

let pp (f : Formatter.t) : t -> unit = function
  | Subject_undefined ->
      Fmt.styled_unit `Cyan "UNDEFINED@ (Subject)" f ()
  | Oracle_undefined ->
      Fmt.styled_unit `Cyan "UNDEFINED@ (Oracle)" f ()
  | Result o ->
      Fmt.pf f "Oracle@ %a@ Subject" Order.pp_operator
        (Au.Set_partial_order.to_ordering_opt o)

let to_string : t -> string = function
  | Subject_undefined ->
      "UNDEFINED (Subject)"
  | Oracle_undefined ->
      "UNDEFINED (Oracle)"
  | Result o ->
      Printf.sprintf "Oracle %s Subject"
        (Order.operator (Au.Set_partial_order.to_ordering_opt o))

let compare_states ~(oracle_states : State.t list)
    ~(subject_states : State.t list) : t =
  let result =
    Tx.Fn.on (Set.of_list (module State))
      ~f:Au.Set_partial_order.make oracle_states subject_states
  in
  Result result

let map_subject_states (states : State.t list)
    ~(location_map : A.Litmus_id.t -> A.Litmus_id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t) : State.t list Or_error.t =
  states
  |> List.map ~f:(State.map ~location_map ~value_map)
  |> Or_error.combine_errors

let domain_error (one_domain : A.Litmus_id.Set.t)
    (another_domain : A.Litmus_id.Set.t) : unit Or_error.t =
  Or_error.error_s
    [%message
      "Domains of states are inconsistent: for example,"
        ~one_domain:(one_domain : A.Litmus_id.Set.t)
        ~another_domain:(another_domain : A.Litmus_id.Set.t)]

let check_domain_consistency (xs_domains : A.Litmus_id.Set.t Sequence.t)
    (x_domain : A.Litmus_id.Set.t) : unit Or_error.t =
  let inconsistencies : unit Or_error.t Sequence.t =
    xs_domains
    |> Sequence.filter ~f:(Fn.non ([%equal: A.Litmus_id.Set.t] x_domain))
    |> Sequence.map ~f:(domain_error x_domain)
  in
  Or_error.combine_errors_unit (Sequence.to_list inconsistencies)

let%test_module "check_domain_consistency expects tests" =
  ( module struct
    let test_x_domain =
      A.Litmus_id.(
        Set.of_list [of_string "0:foo"; of_string "1:bar"; of_string "baz"])

    let%expect_test "no other domains" =
      Stdio.print_s
        [%sexp
          ( check_domain_consistency Sequence.empty test_x_domain
            : unit Or_error.t )] ;
      [%expect {| (Ok ()) |}]

    let%expect_test "only consistent domains" =
      let doms =
        Sequence.of_list (List.init 10 ~f:(Fn.const test_x_domain))
      in
      Stdio.print_s
        [%sexp
          (check_domain_consistency doms test_x_domain : unit Or_error.t)] ;
      [%expect {| (Ok ()) |}]

    let%expect_test "inconsistent domains" =
      let doms =
        Sequence.singleton A.Litmus_id.(Set.of_list [of_string "0:foo"])
      in
      Stdio.print_s
        [%sexp
          (check_domain_consistency doms test_x_domain : unit Or_error.t)] ;
      [%expect
        {|
      (Error
       ("Domains of states are inconsistent: for example,"
        (one_domain (0:foo 1:bar baz)) (another_domain (0:foo)))) |}]
  end )

let get_domain : State.t list -> A.Litmus_id.Set.t Or_error.t = function
  | [] ->
      Or_error.return A.Litmus_id.Set.empty
  | x :: xs ->
      let dom s = A.Litmus_id.Set.of_list (State.bound s) in
      let xs_domains = Sequence.map ~f:dom (Sequence.of_list xs) in
      Tx.Or_error.tee_m (dom x) ~f:(check_domain_consistency xs_domains)

let filter_oracle_states ~(raw_oracle_states : State.t list)
    ~(subject_states : State.t list) : State.t list Or_error.t =
  let open Or_error.Let_syntax in
  let%map domain = get_domain subject_states in
  List.map raw_oracle_states ~f:(State.restrict ~domain)

let run_defined ~(oracle : Ob.t) ~(subject : Ob.t)
    ~(location_map : A.Litmus_id.t -> A.Litmus_id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t) : t Or_error.t =
  let open Or_error.Let_syntax in
  let raw_oracle_states = Ob.states oracle in
  let raw_subject_states = Ob.states subject in
  let%bind subject_states =
    map_subject_states raw_subject_states ~location_map ~value_map
  in
  let%map oracle_states =
    filter_oracle_states ~raw_oracle_states ~subject_states
  in
  compare_states ~oracle_states ~subject_states

let run ~(oracle : Ob.t) ~(subject : Ob.t)
    ~(location_map : A.Litmus_id.t -> A.Litmus_id.t option Or_error.t)
    ~(value_map : string -> string Or_error.t) : t Or_error.t =
  if Ob.is_undefined oracle then Or_error.return Oracle_undefined
  else if Ob.is_undefined subject then Or_error.return Subject_undefined
  else run_defined ~oracle ~subject ~location_map ~value_map
