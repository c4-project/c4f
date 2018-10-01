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

(** Assembly sanitisation *)

open Lang

module type Intf = sig
  type statement

  (** [sanitise stms] sanitises a statement list, turning it into a
     list of separate program lists with various litmus-unfriendly
     elements removed or simplified. *)
  val sanitise : statement list -> statement list list
end

(** [LangHook] is an interface for language-specific hooks into the
   sanitisation process. *)
module type LangHook = sig
  type statement

  val on_program : statement list -> statement list
  val on_statement : statement -> statement
end

(** [NullLangHook] is a [LangHook] that does nothing. *)
module NullLangHook : functor (LS : Language.Intf) ->
                      LangHook with type statement = LS.Statement.t

(** [T] implements the assembly sanitiser for a given language. *)
module T : functor (LS : Language.Intf) ->
           functor (LH : LangHook with type statement = LS.Statement.t) ->
           Intf with type statement := LS.Statement.t

(** [X86] implements x86-specific sanitisation passes.
    It requires an [X86Dialect.Traits] module to tell it things about the
    current x86 dialect (for example, the order of operands). *)
module X86 : functor (DT : X86Dialect.Traits) ->
             LangHook with type statement = X86ATT.Lang.Statement.t
