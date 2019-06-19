(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Common functionality for regress tests *)

open Core_kernel

val regress_on_files :
     string
  -> dir:Fpath.t
  -> ext:string
  -> f:(file:Fpath.t -> path:Fpath.t -> unit Or_error.t)
  -> unit Or_error.t

val regress_run_asm_many :
     (module Act_asm.Runner_intf.S with type cfg = 'cfg)
  -> string
  -> Set.M(Act_sanitiser.Pass_group).t
  -> Fpath.t
  -> config_fn:(Act_delitmus.Aux.t -> 'cfg)
  -> unit Or_error.t

val make_regress_command :
  (Fpath.t -> unit Or_error.t) -> summary:string -> Command.t
