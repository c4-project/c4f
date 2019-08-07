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
module Ob = Observation

module Order_utils = struct
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

  let pp_side_set (side : string) (f : Formatter.t) (set : Set.M(Entry).t) :
      unit =
    if not (Set.is_empty set) then
      Fmt.pf f "@ @[<v 2>In %s only:@ %a@]" side
        (Act_utils.My_format.pp_set Entry.pp)
        set

  let pp_summary (f : Formatter.t) (o : Au.Set_partial_order.M(Entry).t) :
      unit =
    Fmt.pf f "Oracle@ %a@ Subject" pp_operator
      (Au.Set_partial_order.to_ordering_opt o)

  let pp : Au.Set_partial_order.M(Entry).t Fmt.t =
    Au.Set_partial_order.(
      Fmt.(
        box pp_summary
        ++ using in_left_only (pp_side_set "oracle")
        ++ using in_right_only (pp_side_set "subject")))
end

type t =
  { domain: Set.M(Act_common.Litmus_id).t
  ; order: Au.Set_partial_order.M(Entry).t
  ; is_oracle_undefined: bool
  ; is_subject_undefined: bool }
[@@deriving fields]

let pp_undefined (s : string) : bool Fmt.t =
  (* The extra space here is supposed to tack onto the end of an existing
     pretty-printer output. *)
  Fmt.(
    Act_utils.My_format.pp_if
      (const (sp ++ string ++ any "UNDEFINED") s)
      nop)

let pp_domain : Set.M(Act_common.Litmus_id).t Fmt.t =
  Fmt.(
    sp
    ++ vbox ~indent:2
         ( any "Variables considered:"
         ++ cut
         ++ Act_utils.My_format.pp_set Act_common.Litmus_id.pp ))

let pp : t Fmt.t =
  Fmt.(
    vbox
      (concat ~sep:nop
         [ using order Order_utils.pp
         ; using domain pp_domain
         ; using is_oracle_undefined (pp_undefined "oracle")
         ; using is_subject_undefined (pp_undefined "subject") ]))

let to_string : t -> string = Fmt.to_to_string pp

let compare_states ~(oracle_states : Entry.t list)
    ~(subject_states : Entry.t list) : Au.Set_partial_order.M(Entry).t =
  Tx.Fn.on
    (Set.of_list (module Entry))
    ~f:Au.Set_partial_order.make oracle_states subject_states

let get_common_domain (raw_oracle_states : Entry.t list)
    (raw_subject_states : Entry.t list) :
    Set.M(Act_common.Litmus_id).t Or_error.t =
  Or_error.Let_syntax.(
    let%map oracle_domain = Entry.common_domain raw_oracle_states
    and subject_domain = Entry.common_domain raw_subject_states in
    Set.inter oracle_domain subject_domain)

let filter_to_domain (states : Entry.t list)
    ~(domain : Set.M(Act_common.Litmus_id).t) : Entry.t list =
  List.map states ~f:(Entry.restrict ~domain)

let run ~(oracle : Ob.t) ~(subject : Ob.t) : t Or_error.t =
  let raw_oracle_states = Ob.states oracle in
  let raw_subject_states = Ob.states subject in
  Or_error.Let_syntax.(
    let%map domain =
      get_common_domain raw_oracle_states raw_subject_states
    in
    let oracle_states = filter_to_domain raw_oracle_states ~domain in
    let subject_states = filter_to_domain raw_subject_states ~domain in
    let order = compare_states ~oracle_states ~subject_states in
    Fields.create ~domain ~order
      ~is_oracle_undefined:(Ob.is_undefined oracle)
      ~is_subject_undefined:(Ob.is_undefined subject))
