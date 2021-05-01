(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A summary of a configurable fuzzer component. *)

open Base
open Import

(** A wrapper around values assigned to fuzzer component, specifying if it
    was adjusted from its original value. *)
module Adjusted : sig
  (** Type of adjusted weight summaries. *)
  type 'v t =
    | Not_adjusted of 'v  (** The user hasn't overridden this value. *)
    | Adjusted of {original: 'v; actual: 'v}
        (** The value has changed from [original] to [actual]. *)

  val make : 'v option -> default:'v -> 'v t
  (** [make user ~default] makes an adjusted value using the default value
      [default] and optional user adjustment [user]. *)

  (** Adjusted weights may be pretty-printed. *)
  val pp : 'v Fmt.t -> 'v t Fmt.t
  (** [pp pp_v] pretty-prints adjusted values using [pp_v] to print values. *)
end

(** Type of summaries. *)
type 'v t = {value: 'v Adjusted.t; readme: string}

val pp_map : 'v Fmt.t -> string -> 'v t Map.M(Common.Id).t Fmt.t
(** [pp_map pp_v vname] pretty-prints a map of summaries, using [pp_v] for
    the value and [vname] (should be title-case) to describe the value field. *)

val pp_map_terse : 'v Fmt.t -> 'v t Map.M(Common.Id).t Fmt.t
(** [pp_map_terse pp_v] is like {!pp_map}, but only prints names and values. *)
