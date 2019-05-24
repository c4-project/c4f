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

(** Sanitiser: instances of assembly sanitisers.

    This module contains the signatures of full-program assembly sanitisers,
    as well as functors for making them. *)

(** @inline *)
include module type of Instance_intf

(** [Make] implements the assembly sanitiser for a given [Basic]. *)
module Make (B : Basic) :
  S
  with module Lang := B.Hook.Lang
   and type 'a Program_container.t = 'a B.Hook.Program_container.t

(** [Make_single] implements the assembly sanitiser for a given
    [Hook_maker], performing no program splitting and returning the
    sanitised assembly back as one program. *)
module Make_single (H : Hook.S_maker) :
  S
  with module Lang := H(Travesty_containers.Singleton).Lang
   and type 'a Program_container.t = 'a

(** [Make_multi] implements the assembly sanitiser for a given [Hook_maker],
    treating the incoming assembly as holding multiple label-delimited
    programs and splitting them accordingly. *)
module Make_multi (H : Hook.S_maker) :
  S
  with module Lang := H(Travesty_core_kernel_exts.List).Lang
   and type 'a Program_container.t = 'a list
