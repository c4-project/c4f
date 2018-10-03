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

open Core
open Lang

module type WarnIntf =
sig
  module L : Language.Intf

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type t =
    | UnknownElt of elt

  type entry =
    { body     : t
    ; progname : string option
    }
end

module Warn : functor (LS : Language.Intf)
  -> WarnIntf with module L = LS

(** [CtxIntf] is the interface to the state monad used by the sanitiser to
    carry global information around in a sanitisation pass. *)
module type CtxIntf =
sig
  module Warn : WarnIntf

  type ctx =
    { progname : string option
    ; jsyms    : Language.SymSet.t
    ; warnings : Warn.entry list
    }

  type 'a t

  val initial : ctx

  (** Constructing context-sensitive computation s*)

  (** [make] creates a context-sensitive computation that can modify
     the current context. *)
  val make : (ctx -> (ctx * 'a)) -> 'a t

  (** [peek] creates a context-sensitive computation that can look at
     the current context, but not modify it. *)
  val peek : (ctx -> 'a) -> 'a t

  (** [modify] creates a context-sensitive computation that can look at
     and modify the current context, but not modify the value passing
     through. *)
  val modify : (ctx -> ctx) -> 'a -> 'a t

  (** [run] unfolds a [t] into a function from context
      to context and final result. *)
  val run : 'a t -> ctx -> (ctx * 'a)

  (** [run'] behaves like [run], but discards the final context. *)
  val run' : 'a t -> ctx -> 'a

  (** [warn w a] adds a warning [w] to the current context, passing
      [a] through. *)
  val warn : Warn.t -> 'a -> 'a t

  (** [CtxIntf] implementations form monads. *)
  module Monad : Monad.S with type 'a t := 'a t
end

(** [Ctx] builds a context monad for the given language. *)
module Ctx : functor (LS : Language.Intf)
  -> CtxIntf with module Warn.L = LS

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
  module L : Language.Intf
  module C : CtxIntf with module Warn.L = L

  (** [on_program] is a hook mapped over each the program as a
     whole. *)
  val on_program : L.Statement.t list -> (L.Statement.t list) C.t

  (** [on_statement] is a hook mapped over each statement in the
     program. *)
  val on_statement : L.Statement.t -> L.Statement.t C.t

  (** [on_instruction] is a hook mapped over each instruction in the
     program. *)
  val on_instruction : L.Instruction.t -> L.Instruction.t C.t

  (** [on_location] is a hook mapped over each location in the
      program. *)
  val on_location : L.Location.t -> L.Location.t C.t
end

(** [NullLangHook] is a [LangHook] that does nothing. *)
module NullLangHook : functor (LS : Language.Intf)
  -> LangHook with module L = LS

(** [T] implements the assembly sanitiser for a given language. *)
module T :
  functor (LS : Language.Intf)
    -> functor (LH : LangHook with module L = LS)
    -> Intf with type statement := LS.Statement.t

(** [X86] implements x86-specific sanitisation passes.
    It requires an [X86Dialect.Traits] module to tell it things about the
    current x86 dialect (for example, the order of operands). *)
module X86 : functor (DT : X86Dialect.Traits)
  -> LangHook with module L = X86ATT.Lang
