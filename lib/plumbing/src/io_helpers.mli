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

(** Helper functions for dealing with various patterns of {{!Input} Input}
    and {{!Output} Output} usage. *)

open Base
open Stdio

val with_input_and_output :
     Input.t
  -> Output.t
  -> f:(In_channel.t -> Out_channel.t -> 'a Or_error.t)
  -> 'a Or_error.t
(** [with_input_and_output i o ~f] runs [f] with the appropriate channels
    pointed to by [i] and [o]. *)

val lift_to_raw_strings :
     f:('i -> Input.t -> Output.t -> 'o Or_error.t)
  -> 'i
  -> infile:string option
  -> outfile:string option
  -> 'o Or_error.t

val lift_to_fpaths :
     f:('i -> Input.t -> Output.t -> 'o Or_error.t)
  -> 'i
  -> infile:Fpath.t option
  -> outfile:Fpath.t option
  -> 'o Or_error.t
