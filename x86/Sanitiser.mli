(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** x86-specific functionality for act's sanitiser *)

(** [Hook] implements x86-specific sanitisation passes.
    It requires an [Language.Intf] module to tell it things about the
    current x86 dialect (for example, the order of operands). *)
module Hook
  : functor (L : Language.S)
    -> Lib.Sanitiser.Hook with module Lang := L

(** [Make_single] directly instantiates a single-program sanitiser for an
    [Language.Intf] module. *)
module Make_single
  : functor (L : Language.S)
    -> Lib.Sanitiser.S with module Lang := L
                        and type 'a Program_container.t = 'a
;;


(** [Make_multi] directly instantiates a multi-program sanitiser for anb
    [Language.Intf] module. *)
module Make_multi
  : functor (L : Language.S)
    -> Lib.Sanitiser.S with module Lang := L
                        and type 'a Program_container.t = 'a list
;;

