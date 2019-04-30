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

(** Assembly sanitisation *)

include module type of Sanitiser_intf

(** [Make_null_hook] makes a [Hook] that does nothing. *)
module Make_null_hook
    (Lang : Language.S)
    (P : Travesty.Traversable.S1_container) :
  Hook with module Lang = Lang and module Program_container = P

(** [Make] implements the assembly sanitiser for a given [Basic]. *)
module Make (B : Basic) :
  S
  with module Lang := B.Lang
   and type 'a Program_container.t = 'a B.Program_container.t

(** [Make_single] implements the assembly sanitiser for a given
    [Hook_maker], performing no program splitting and returning the
    sanitised assembly back as one program. *)
module Make_single (H : Hook_maker) :
  S
  with module Lang := H(Travesty.Singleton).Lang
   and type 'a Program_container.t = 'a

(** [Make_multi] implements the assembly sanitiser for a given [Hook_maker],
    treating the incoming assembly as holding multiple label-delimited
    programs and splitting them accordingly. *)
module Make_multi (H : Hook_maker) :
  S
  with module Lang := H(Travesty_core_kernel_exts.List).Lang
   and type 'a Program_container.t = 'a list
