(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

(* This module gymnastics brought to you by needing to have a comparator
   module below. *)
module M = struct
  module M_inner = struct
    type t = In_loop | In_dead_code | In_atomic [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [ (In_loop, "in loop")
      ; (In_dead_code, "in dead code")
      ; (In_atomic, "in atomic block") ]
  end

  include M_inner
  include Act_utils.Enum.Extend_table (M_inner)
end

include M

(** Maps a subset of the flags to predicates that toggle whether a piece of
    metadata sets the flag or not. *)
let metadata_predicates : (t, Metadata.t -> bool) List.Assoc.t =
  [(In_dead_code, Metadata.is_dead_code)]

let flags_of_flow : Act_fir.Statement_class.Flow.t -> Set.M(M).t = function
  | While _ ->
      Set.singleton (module M) In_loop
  | Lock (Some Atomic) ->
      Set.singleton (module M) In_atomic
  | Lock _ ->
      Set.empty (module M)

let flags_of_metadata (m : Metadata.t) : Set.M(M).t =
  metadata_predicates
  |> List.filter_map ~f:(fun (flag, f) -> Option.some_if (f m) flag)
  |> Set.of_list (module M)
