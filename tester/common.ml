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

open Base
open Act_common
open Lib

include Common_intf

module Extend (B : Basic) : Extensions with type 'a timed := 'a B.T.t = struct
  module TS = Timing_set.Make (B.T)

  let bracket ?(id : Id.t = Id.of_string "none")
      (f : unit -> 'a Or_error.t) ~(stage : string) ~(file : string) : 'a B.T.t Or_error.t =
    (* TODO (@MattWindsor91): make id properly optional. *)

    Output.log_stage B.o ~stage ~file id ;
    let f' () =
      Or_error.tag_arg (f ()) "While executing tester stage" stage
        String.sexp_of_t
    in
    B.T.bracket_join f'
end
