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

module Location_map = struct
  type t = A.Litmus_id.t option Map.M(A.Litmus_id).t

  module Json = Plumbing.Jsonable.Make_map (A.Litmus_id) (Plumbing.Jsonable.Option(A.Litmus_id))
  include (Json : module type of Json with type t := t)
  module Load = Plumbing.Loadable.Of_jsonable (Json)
  include (Load : module type of Load with type t := t)

  let reflexive
    (vars : Set.M(Act_common.Litmus_id).t)
  : t =
    vars
    |> Set.to_sequence ~order:`Increasing
    |> Sequence.map ~f:(fun x -> (x, Some x))
    |> Map.of_increasing_sequence (module Act_common.Litmus_id)
    |> Or_error.ok_exn

  let output (map : t) ~(onto : Plumbing.Output.t) : unit Or_error.t =
    Plumbing.Output.with_output onto
      ~f:(fun oc ->
          map
          |> to_yojson
          |> Yojson.Safe.pretty_to_channel oc;
          Stdio.Out_channel.newline oc;
          Result.ok_unit)
end

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

let pp_undefined : string Fmt.t =
  Fmt.(styled (`Fg `Cyan) (any "UNDEFINED" ++ sp ++ string))

let pp_summary (f : Formatter.t) : t -> unit = function
  | Subject_undefined ->
      pp_undefined f "Subject"
  | Oracle_undefined ->
      pp_undefined f "Oracle"
  | Result o ->
      Fmt.pf f "Oracle@ %a@ Subject" Order.pp_operator
        (Au.Set_partial_order.to_ordering_opt o)

let pp_side_set (side : string) (f : Formatter.t) (set : Set.M(State).t) :
    unit =
  if not (Set.is_empty set) then
    Fmt.(
      Fmt.pf f "@[<v 2>In %s only:@ %a@]" side
        (using Set.to_list (list ~sep:cut State.pp))
        set)

let pp_specifics (f : Formatter.t) : t -> unit = function
  | Result o ->
      Au.Set_partial_order.(
        Fmt.(
          using in_left_only (pp_side_set "oracle")
          ++ (if is_unordered o then sp else nop)
          ++ using in_right_only (pp_side_set "subject")))
        f o
  | Subject_undefined | Oracle_undefined ->
      ()

let pp : t Fmt.t = Fmt.(vbox (box pp_summary ++ sp ++ box pp_specifics))

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

let not_in_subject_error (location : A.Litmus_id.t) : Error.t Lazy.t =
  (* TODO(@MattWindsor91): is this even an error? are there examples where
     this legitimately happens eg. compiler optimisations? *)
  lazy
    (Error.create_s
       [%message
         "Location present in the oracle but absent in the subject"
           ~location:(location : A.Litmus_id.t)])

let map_subject_states (states : State.t list)
    ~(location_map : Location_map.t) =
  Tx.Or_error.combine_map states
    ~f:
      (State.map ~value_map:Or_error.return ~location_map:(fun l ->
           l |> Map.find location_map
           |> Result.of_option
                ~error:Error.(of_lazy_t (not_in_subject_error l))))

let filter_oracle_states ~(raw_oracle_states : State.t list)
    ~(subject_states : State.t list) : State.t list Or_error.t =
  let open Or_error.Let_syntax in
  let%map domain = State.common_domain subject_states in
  List.map raw_oracle_states ~f:(State.restrict ~domain)

let run_defined ~(oracle : Ob.t) ~(subject : Ob.t)
    ~(location_map : Location_map.t) :
    t Or_error.t =
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
    ~(location_map : Location_map.t) :
    t Or_error.t =
  if Ob.is_undefined oracle then Or_error.return Oracle_undefined
  else if Ob.is_undefined subject then Or_error.return Subject_undefined
  else run_defined ~oracle ~subject ~location_map
