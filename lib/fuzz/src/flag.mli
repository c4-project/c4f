(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Values on Boolean flags.

    Boolean flag values [f] take the form of an expression of the odds on the
    flag being set to [true] when [eval f ~random] is run. *)

open Base

type t [@@deriving sexp]
(** Opaque type of flag values. *)

(** {1 Interface implementations} *)

include Pretty_printer.S with type t := t
(** One can pretty-print a flag value [f]. The pretty-printed representation
    is [Bool.pp b] when [f] is an exact [b], and 'W:L odds on' (where W is
    [wins f] and L is [losses f]) otherwise. *)

(** {1 Constructors} *)

val try_make : wins:int -> losses:int -> t Or_error.t
(** [try_make ~wins ~losses] tries to construct a flag value with odds
    expressed as [wins] to [losses] on. It fails if either [wins] or [losses]
    is negative, or their sum is [0]. *)

val exact : bool -> t
(** [exact b] is a flag value [v] for which [eval v ~random] always returns
    [b]. *)

(** {1 Accessors} *)

val eval : t -> random:Splittable_random.State.t -> bool
(** [eval f ~random] evaluates [f] to a truth value, using [random] as a
    random number generator if necessary. *)

val to_exact_opt : t -> bool option
(** [to_exact_opt f] returns [Some b] if [eval f ~random] will always return
    the same Boolean [b], and [None] otherwise. *)

(** {2 Extracting the raw odds}

    These accessors mainly exist to allow one to convert the flag value back
    to its in-config format. *)

val wins : t -> int
(** [wins f] gets the number of times, on average, that [(wins f + losses f)]
    instances of [eval f ~random] would return [true]. *)

val losses : t -> int
(** [wins f] gets the number of times, on average, that [(wins f + losses f)]
    instances of [eval f ~random] would return [true]. *)
