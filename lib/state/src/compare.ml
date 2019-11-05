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

module Result = struct
  module Domain = Plumbing.Jsonable.Set.Make (Act_common.Litmus_id)

  module Ordering = struct
    type t = Ordering.t

    let yojson_of_t : t -> Yojson.Safe.t = function
      | Equal ->
          `String "=="
      | Less ->
          `String "<"
      | Greater ->
          `String ">"
  end

  type t =
    { domain: Domain.t
    ; diff: Diff.t
    ; order: Ordering.t option
    ; is_oracle_undefined: bool
    ; is_subject_undefined: bool }
  [@@deriving fields, yojson_of]

  let pp_json (f : Formatter.t) (result : t) : unit =
    result |> yojson_of_t |> Yojson.Safe.pretty_print f

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

  let colour : Ordering.t option -> Fmt.style = function
    | Some Equal ->
        `Fg `Green
    | Some Less ->
        `Fg `Red
    | Some Greater ->
        `Fg `Yellow
    | None ->
        `Fg `Magenta

  let operator : Ordering.t option -> string = function
    | Some Equal ->
        "=="
    | Some Less ->
        "<<"
    | Some Greater ->
        ">>"
    | None ->
        "<>"

  let pp_operator (f : Formatter.t) (op : Ordering.t option) : unit =
    Fmt.(styled (colour op) (using operator string)) f op

  let pp_order (f : Formatter.t) (o : Ordering.t option) : unit =
    Fmt.pf f "@[<h>Oracle@ %a@ Subject@]" pp_operator o

  let pp : t Fmt.t =
    (* Some of these fields are optional, so we can't easily just concatenate
       them all with spaces. *)
    Fmt.(
      vbox
        (concat ~sep:nop
           [ using order pp_order
           ; using
               (Fn.compose (Fn.non Diff.is_equal) diff)
               (Act_utils.My_format.pp_if sp nop)
           ; using diff Diff.pp
           ; using domain pp_domain
           ; using is_oracle_undefined (pp_undefined "oracle")
           ; using is_subject_undefined (pp_undefined "subject") ]))

  let to_string : t -> string = Fmt.to_to_string pp
end

let get_common_domain (raw_oracle_states : Set.M(Entry).t)
    (raw_subject_states : Set.M(Entry).t) :
    Set.M(Act_common.Litmus_id).t Or_error.t =
  Or_error.Let_syntax.(
    let%map oracle_domain =
      Entry.common_domain (Set.to_list raw_oracle_states)
    and subject_domain =
      Entry.common_domain (Set.to_list raw_subject_states)
    in
    Set.inter oracle_domain subject_domain)

let filter_to_domain (states : Set.M(Entry).t)
    ~(domain : Set.M(Act_common.Litmus_id).t) : Set.M(Entry).t =
  Set.map (module Entry) states ~f:(Entry.restrict ~domain)

let run ~(oracle : Ob.t) ~(subject : Ob.t) : Result.t Or_error.t =
  let raw_oracle_states = Ob.states oracle in
  let raw_subject_states = Ob.states subject in
  Or_error.Let_syntax.(
    let%map domain =
      get_common_domain raw_oracle_states raw_subject_states
    in
    let oracle_states = filter_to_domain raw_oracle_states ~domain in
    let subject_states = filter_to_domain raw_subject_states ~domain in
    let diff = Diff.make oracle_states subject_states in
    Result.Fields.create ~domain ~diff
      ~order:(Diff.to_ordering_opt diff)
      ~is_oracle_undefined:(Ob.is_undefined oracle)
      ~is_subject_undefined:(Ob.is_undefined subject))
