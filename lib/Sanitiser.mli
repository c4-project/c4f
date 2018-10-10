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
open Utils

(*
 * Warnings
 *)

(** [CustomWarnS] is the signature that any custom sanitisation
   warnings need to implement. *)
module type CustomWarnS = sig
  include Pretty_printer.S
end

(** [NoCustomWarn] is a dummy interface implementing [CustomWarnS]. *)
module NoCustomWarn : CustomWarnS

(** [WarnIntf] is the interface to the warnings emitted by the
   sanitiser. *)
module type WarnIntf = sig
  module L : Language.Intf
  module C : CustomWarnS

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t

  type body =
    | UnknownElt of elt
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string option
    }

  include Pretty_printer.S with type t := t
end

(** [Warn] produces a warnings module for the given language and
   custom warnings set. *)
module Warn
  : functor (L : Language.Intf)
    -> functor (C : CustomWarnS)
      -> WarnIntf with module L = L and module C = C

(*
 * Context
 *)

(** [CtxIntf] is the interface to the state monad used by the sanitiser to
    carry global information around in a sanitisation pass. *)
module type CtxIntf = sig
  module Warn : WarnIntf

  type ctx =
    { progname : string option
    ; hsyms    : Language.SymSet.t  (* Heap symbols *)
    ; jsyms    : Language.SymSet.t  (* Jump symbols *)
    ; warnings : Warn.t list
    }


  (** [CtxIntf] implementations form monads. *)
  include Monad.S

  (** [CtxIntf] implementations also include some monad extensions. *)
  include MyMonad.Extensions with type 'a t := 'a t

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
  val warn : Warn.body -> 'a -> 'a t
end

(** [CtxMake] builds a context monad for the given language. *)
module CtxMake
  : functor (L : Language.Intf)
    -> functor (C : CustomWarnS)
      -> CtxIntf with module Warn.L = L and module Warn.C = C

(*
 * Language-dependent parts
 *)

(** [LangHookS] is an interface for language-specific hooks into the
    sanitisation process. *)
module type LangHookS = sig
  module L : Language.Intf
  module Ctx : CtxIntf with module Warn.L = L

  (** [on_program] is a hook mapped over each the program as a
     whole. *)
  val on_program : L.Statement.t list -> (L.Statement.t list) Ctx.t

  (** [on_statement] is a hook mapped over each statement in the
     program. *)
  val on_statement : L.Statement.t -> L.Statement.t Ctx.t

  (** [on_instruction] is a hook mapped over each instruction in the
     program. *)
  val on_instruction : L.Instruction.t -> L.Instruction.t Ctx.t

  (** [on_location] is a hook mapped over each location in the
      program. *)
  val on_location : L.Location.t -> L.Location.t Ctx.t
end

(** [NullLangHook] is a [LangHook] that does nothing. *)
module NullLangHook : functor (LS : Language.Intf)
  -> LangHookS with module L = LS

(*
 * Main sanitiser modules
 *)

module type Intf = sig
  module Warn : WarnIntf

  type statement

  (** [output] is the type of (successful) sanitiser output. *)
  type output =
    { programs : statement list list
    ; warnings : Warn.t list
    }

  (** [sanitise stms] sanitises a statement list, turning it into a
      list of separate program lists with various litmus-unfriendly
      elements removed or simplified. *)
  val sanitise : statement list -> output
end

(** [Make] implements the assembly sanitiser for a given language. *)
module Make :
  functor (LH : LangHookS)
    -> Intf with type statement = LH.L.Statement.t
