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

open Core_kernel
open Act_utils
include Timing_set_intf

module Make (T : Timing.S) : S with type 'a input := 'a T.t = struct
  type elt = Time.Span.t

  type t =
    { in_delitmusifier: elt option
    ; in_c_simulator: elt option
    ; in_compiler: elt option
    ; in_litmusifier: elt option
    ; in_asm_simulator: elt option }
  [@@deriving fields]

  let make ?(delitmusifier : _ T.t option) ?(c_simulator : _ T.t option)
      ?(compiler : _ T.t option) ?(litmusifier : _ T.t option)
      ?(asm_simulator : _ T.t option) () : t =
    Option.
      { in_delitmusifier= delitmusifier >>= T.time_taken
      ; in_c_simulator= c_simulator >>= T.time_taken
      ; in_compiler= compiler >>= T.time_taken
      ; in_litmusifier= litmusifier >>= T.time_taken
      ; in_asm_simulator= asm_simulator >>= T.time_taken }
end
