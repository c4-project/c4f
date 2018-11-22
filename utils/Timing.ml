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

module type Timed1 = sig
  type 'a t
  val time_taken : 'a t -> Time.Span.t option
end

module type Timed0 = sig
  type t
  include Timed1 with type 'a t := t
end

module type Timer = sig
  val time : unit -> Time.t option
end

module Null_timer : Timer = struct
  let time = Fn.const None
end

module Now_timer : Timer = struct
  let time () = Some (Time.now ())
end

module With_timing (T : Timer) = struct
  type 'a t =
    { value      : 'a
    ; time_taken : Time.Span.t option
    }
  [@@deriving fields]
  ;;

  let make_span pre post = Option.map2 ~f:Time.diff post pre

  let bracket thunk =
    let pre   = T.time () in
    let value = thunk  () in
    let post  = T.time () in
    let time_taken = make_span pre post in
    { value; time_taken }
  ;;
end
