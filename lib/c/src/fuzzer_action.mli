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

(** Fuzzer: high-level actions *)

open Base
open Act_common

include module type of Fuzzer_action_intf

(** {2 Summaries of actions}

    To build these summaries, see {{!Pool.summarise} Pool.summarise} below. *)

(** A summary of the weight assigned to an action in an action pool. *)
module Adjusted_weight : sig
  (** Type of adjusted weight summaries. *)
  type t =
    | Not_adjusted of int  (** The user hasn't overridden this weight. *)
    | Adjusted of {original: int; actual: int}
        (** The weight has changed from [original] to [actual]. *)

  (** Adjusted weights may be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** A summary of a fuzzer action after weight adjustment. *)
module Summary : sig
  (** Opaque type of summaries. *)
  type t

  val weight : t -> Adjusted_weight.t
  (** [weight summary] gets the final 'adjusted' weight of the action
      described by [summary]. *)

  val readme : t -> string
  (** [readme summary] gets the README of the action described by [summary]. *)

  (** Summaries may be pretty-printed. *)
  include Pretty_printer.S with type t := t
end

(** A weighted pool of fuzzer actions. *)
module Pool : sig
  (** Action lists are just weighted lists of first-class action modules. *)
  type t

  val make : (module S) list -> Act_config.Fuzz.t -> t Or_error.t
  (** [make actions config] tries to make a weighted action pool by taking
      the actions defined in [action_map] and applying the user-specified
      weight overrides in [config]. *)

  val summarise : t -> Summary.t Id.Map.t
  (** [summarise pool] generates a mapping from action names to summaries of
      each action, including its adjusted weight in the pool. *)

  val pick :
       t
    -> Fuzzer_subject.Test.t
    -> Splittable_random.State.t
    -> (module S) Fuzzer_state.Monad.t
  (** [pick pool subject rng] is a stateful action that picks a
      weighted-random action module from [pool] that is available on
      [subject], using [pool] as a random number generator. *)
end
