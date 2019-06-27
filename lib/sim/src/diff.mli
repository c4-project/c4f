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

(** Comparing two simulation outputs.

    This module contains a function, {{!run} run}, for running
    'diff'-comparisons between two simulation outputs: an 'oracle'
    (generally from a C litmus test) and a 'subject' (generally from its
    compiled assembly).

    Each run takes, as additional parameters, partial mappings between the
    (litmus-style) state variable identifiers, and (uninterpreted string)
    values, mapping from the subject back to the oracle. *)

open Base

(** Synonym of [Set_partial_order] for orderings between state sets. *)
module Order : sig
  type t = (State.t, State.comparator_witness) Act_utils.Set_partial_order.t
end

type t =
  | Oracle_undefined
      (** The oracle execution triggered undefined behaviour. *)
  | Subject_undefined
      (** The subject execution triggered undefined behaviour. *)
  | Result of Order.t  (** Analysis completed with the following result. *)

val to_string : t -> string
(** [to_string result] returns a human-readable string representing
    [result]. *)

include
  Pretty_printer.S with type t := t
(** We can also pretty-print diff results, with similar results to running
    [to_string]. *)

val run :
     oracle:Output.Observation.t
  -> subject:Output.Observation.t
  -> location_map:(   Act_common.Litmus_id.t
                   -> Act_common.Litmus_id.t option Or_error.t)
  -> value_map:(string -> string Or_error.t)
  -> t Or_error.t
(** [run ~oracle ~subject ~location_map ~value_map] applies the partial
    mappers [location_map] and [value_map] to every state binding in
    [subject], then analyses it against [oracle]. *)
