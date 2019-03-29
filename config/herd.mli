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

(** Configuration needed to find and execute Herd. *)

include Program.S

(** {2 Constructors} *)

val make :
     ?cmd:string
  -> ?c_model:string
  -> ?asm_models:(Id.t, string) List.Assoc.t
  -> unit
  -> t

(** {2 Accessors}

    These accessors complement the existing ones pulled in by
    {{!Program.S} Program.S}. *)

val c_model : t -> string option
(** [c_models cfg] gets the configured C model overrides in [cfg], if one
    exists. *)

val asm_models : t -> (Id.t, string) List.Assoc.t
(** [asm_models cfg] gets the list of configured assembly model overrides in
    [cfg]. Each override maps an architecture, represented by its config ID,
    to a path. *)
