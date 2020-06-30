(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Shc = Act_backend_herdtools_common
module Tag = Act_state.Observation.Entry_tag

(* Map over an optional tuple. *)
module O2 = Travesty.Bi_mappable.Chain_Bi2_Map1 (Tx.Tuple2) (Option)

let split_off_metadata (line : string) : string option * string =
  line |> String.lsplit2 ~on:'>'
  |> O2.bi_map
       ~left:(Fn.compose Option.some String.strip)
       ~right:String.strip
  |> Option.value ~default:(None, line)

let split_in_metadata : string option -> string option * string option =
  function
  | None ->
      (None, None)
  | Some metadata_str ->
      let occurrences_str = String.(rstrip (drop_suffix metadata_str 1)) in
      let tag_str = String.suffix metadata_str 1 in
      (Some occurrences_str, Some tag_str)

let split_line_to_string_tuple (line : string) :
    string option * string option * string =
  let metadata_str, rest = split_off_metadata line in
  let occurrences_str, tag_str = split_in_metadata metadata_str in
  (occurrences_str, tag_str, rest)

let parse_int (s : string) : int Or_error.t =
  Or_error.try_with (fun () -> Int.of_string s)

let parse_int_opt : string option -> int option Or_error.t =
  Tx.Option.With_errors.map_m ~f:parse_int

let parse_tag (tt : Shc.Reader.Test_type.t) (tag : string) : Tag.t Or_error.t
    =
  match (tt, tag) with
  | Allowed, "*" | Required, ":" ->
      Or_error.return Tag.Witness
  | Allowed, ":" | Required, "*" ->
      Or_error.return Tag.Counter_example
  | _ ->
      Or_error.error_s
        [%message "Unknown tag in Litmus histogram" (tag : string)]

let parse_tag_opt (tt : Shc.Reader.Test_type.t) :
    string option -> Tag.t option Or_error.t =
  Tx.Option.With_errors.map_m ~f:(parse_tag tt)

include Shc.Reader.Make (struct
  let try_parse_state_count (line : string) : int option =
    Option.try_with (fun () ->
        Caml.Scanf.sscanf line "Histogram (%d states)" Fn.id )

  let try_split_state_line (tt : Shc.Reader.Test_type.t) (line : string) :
      string Shc.Reader.State_line.t Or_error.t =
    let occurrences_str, tag_str, rest = split_line_to_string_tuple line in
    Or_error.Let_syntax.(
      let%map occurrences = parse_int_opt occurrences_str
      and tag = parse_tag_opt tt tag_str in
      Shc.Reader.State_line.make ?occurrences ?tag rest)
end)
