(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Result = struct
  type t = Entry.t Act_litmus.Postcondition_eval.Result.t
  [@@deriving yojson_of]
end

let check_elt (state : Entry.t) :
    string Act_litmus.Predicate.Element.t -> bool = function
  | Bool k ->
      k
  | Eq (key, data) ->
      Entry.maps_to ~key ~data state

let run (obs : Observation.t) ~(post : string Act_litmus.Postcondition.t) :
    Result.t =
  let states = Observation.states obs in
  let subjects = Set.to_list states in
  Act_litmus.Postcondition_eval.eval post ~subjects ~elt:check_elt
