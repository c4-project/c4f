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

(** High-level form of `asm` command targets. *)

open Base

(** Type of top-level command targets, as taken from the command line. *)
type t = Arch of Act_common.Id.t | Compiler_id of Act_common.Id.t

val arch : Act_common.Id.t -> t
(** [arch id] is [Arch id]. *)

val compiler_id : Act_common.Id.t -> t
(** [compiler_id id] is [Compiler_id id]. *)

val resolve : t -> cfg:Act_config.Act.t -> Act_machine.Target.t Or_error.t
(** [resolve target ~cfg] passes through [target] if it's a direct
    architecture reference; if it's a compiler ID, it tries to look up that
    ID in [cfg], resolving it to a compiler spec. *)
