(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module A = Act_common
module Au = Act_utils
module Tx = Travesty_base_exts
module Ob = Output.Observation

module Order = struct
  include Au.Set_partial_order.M (State)

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
        `Fg `Green
    | Some Less ->
        `Fg `Red
    | Some Greater ->
        `Fg `Yellow
    | None ->
        `Fg `Magenta

  let pp_operator (f : Formatter.t) (op : Ordering.t option) : unit =
    Fmt.(styled (colour op) (using operator string)) f op
end

type t = Oracle_undefined | Subject_undefined | Result of Order.t

let pp_summary (f : Formatter.t) : t -> unit = function
  | Subject_undefined ->
      Fmt.styled (`Fg `Cyan) (Fmt.any "UNDEFINED@ (Subject)") f ()
  | Oracle_undefined ->
      Fmt.styled (`Fg `Cyan) (Fmt.any "UNDEFINED@ (Oracle)") f ()
  | Result o ->
      Fmt.pf f "Oracle@ %a@ Subject" Order.pp_operator
        (Au.Set_partial_order.to_ordering_opt o)

let pp_side_set (f : Formatter.t) (side : string) (set : Set.M(State).t) : unit =
  if not (Set.is_empty set)
  then Fmt.(
      Fmt.pf f "@ @[<v 2>In %s only:@ %a@]"
        side
        (using Set.to_list (list ~sep:cut State.pp))
        set)

let pp_specifics (f : Formatter.t) : t -> unit = function
  | Result o ->
    pp_side_set f "oracle" (Au.Set_partial_order.in_left_only o);
    pp_side_set f "subject" (Au.Set_partial_order.in_right_only o)
  | Subject_undefined | Oracle_undefined -> ()

let pp : t Fmt.t =
  Fmt.(vbox (box pp_summary ++ sp ++ box pp_specifics))


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
    Tx.Fn.on
      (Set.of_list (module State))
      ~f:Au.Set_partial_order.make oracle_states subject_states
  in
  Result result

let map_subject_states (states : State.t list)
    ~(location_map : A.Litmus_id.t option Map.M(A.Litmus_id).t) =
  (* TODO(@MattWindsor91): is this even an error?  are there examples where this
     legitimately happens eg. compiler optimisations? *)
    Tx.Or_error.combine_map states ~f:(
    State.map
      ~value_map:Or_error.return
      ~location_map:(fun l ->
          l |> Map.find location_map |>
          Result.of_option
            ~error:(Error.(of_lazy_t
                             (lazy
                               (create_s
                               [%message "Location present in the oracle but absent in the subject"
                                       ~location:(l : A.Litmus_id.t)]
                             ))))))

let filter_oracle_states ~(raw_oracle_states : State.t list)
    ~(subject_states : State.t list) : State.t list Or_error.t =
  let open Or_error.Let_syntax in
  let%map domain = State.common_domain subject_states in
  List.map raw_oracle_states ~f:(State.restrict ~domain)

let run_defined ~(oracle : Ob.t) ~(subject : Ob.t)
    ~(location_map : A.Litmus_id.t option Map.M(A.Litmus_id).t)
  : t Or_error.t =
  let open Or_error.Let_syntax in
  let raw_oracle_states = Ob.states oracle in
  let raw_subject_states = Ob.states subject in
  let%bind subject_states =
    map_subject_states raw_subject_states ~location_map
  in
  let%map oracle_states =
    filter_oracle_states ~raw_oracle_states ~subject_states
  in
  compare_states ~oracle_states ~subject_states

let run ~(oracle : Ob.t) ~(subject : Ob.t)
    ~(location_map : A.Litmus_id.t option Map.M(A.Litmus_id).t) : t Or_error.t =
  if Ob.is_undefined oracle then Or_error.return Oracle_undefined
  else if Ob.is_undefined subject then Or_error.return Subject_undefined
  else run_defined ~oracle ~subject ~location_map
