(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let predicate_of_state (entry : Entry.t) : string Act_litmus.Predicate.t =
  entry |> Entry.to_alist |> Sequence.of_list
  |> Sequence.map ~f:(fun (k, v) -> Act_litmus.Predicate.eq k v)
  |> Act_litmus.Predicate.optimising_and_seq

let predicate_of_states (states : Set.M(Entry).t) :
    string Act_litmus.Predicate.t =
  states |> Set.to_sequence
  |> Sequence.map ~f:predicate_of_state
  |> Act_litmus.Predicate.optimising_or_seq

let convert_states (entries : Set.M(Entry).t) :
    string Act_litmus.Postcondition.t =
  Act_litmus.Postcondition.make ~quantifier:For_all
    ~predicate:(predicate_of_states entries)

let convert : Observation.t -> string Act_litmus.Postcondition.t =
  Fn.compose convert_states Observation.states

let print_postcondition (output : Plumbing.Output.t) :
    string Act_litmus.Postcondition.t -> unit Or_error.t =
  Act_utils.My_format.odump output
    (Fmt.hbox (Act_litmus.Postcondition.pp ~pp_const:String.pp))

module Filter = struct
  let run (input : Plumbing.Input.t) (output : Plumbing.Output.t) :
      unit Or_error.t =
    Or_error.(
      input |> Observation.load >>| convert >>= print_postcondition output)
end
