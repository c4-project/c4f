(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel

(** [Timed1] is a signature for arity-1 things that contain a
   timestamp. *)
module type Timed1 = sig
  (** [t] is the type of something being timed. *)
  type 'a t

  (** [time_taken t] gets a record of the time spent processing [t]. *)
  val time_taken : 'a t -> Time.Span.t option
end

(** [Timed0] is a signature for arity-0 things that contain a
   timestamp. *)
module type Timed0 = sig
  type t
  include Timed1 with type 'a t := t
end

(** [Timer] is a signature for things that can measure times. *)
module type Timer = sig
  (** [time ()] returns the current time, if this [Timer] supports
      doing so. *)
  val time : unit -> Time.t option
end

(** [Null_timer] is an implemention of [Timer] that returns
    [None] for each invocation of [time]. *)
module Null_timer : Timer

(** [Now_timer] is an implementation of [Timer] that uses
    [Time.now] for [time]. *)
module Now_timer : Timer

(** [With_timing] is a container associating an arbitrary type with a
    timestamp. *)
module With_timing : functor (T : Timer) -> sig
  type 'a t

  (** [value t] gets the value inside the [With_timing] bracket. *)
  val value : 'a t -> 'a

  (** [bracket thunk] executes [thunk], using [T.time] to measure
      timestamps before and after execution. *)
  val bracket : (unit -> 'a) -> 'a t

  include Timed1 with type 'a t := 'a t
end
