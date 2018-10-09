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

(** Modules implementing x86-specific functionality for act

    This module shouldn't be confused with the x86 modules in [Lang],
   which are intended to be less dependent on what act actually
   does. *)

open Lang

(** [SanitiserHook] implements x86-specific sanitisation passes.
    It requires an [X86.Lang] module to tell it things about the
    current x86 dialect (for example, the order of operands). *)
module SanitiserHook
  : functor (L : X86.Lang) -> Sanitiser.LangHookS with module L = L

(** [Sanitiser] directly instantiates a sanitiser for an
    [X86.Lang] module. *)
module Sanitiser
  : functor (L : X86.Lang)
    -> Sanitiser.Intf with type statement = L.Statement.t

(** [LitmusDirect] instantiates a litmus test emitter for the Herd7
   dialect of x86. *)
module LitmusDirect
  : Litmus.Intf with type LS.Statement.t = X86Ast.statement
