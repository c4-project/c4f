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
    (* Something needed an end-of-program label, but there wasn't one. *)
    | MissingEndLabel
    (* This element isn't known to act, and its translation may be wrong. *)
    | UnknownElt of elt
    (* Hook for language-specific sanitiser passes to add warnings. *)
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string
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
 * Passes
 *)

(** [Pass] enumerates the various sanitisation passes. *)
module Pass : sig
  type t =
    (* Run language-specific hooks. *)
    | LangHooks
    (* Mangle symbols to ensure litmus tools can lex them. *)
    | MangleSymbols
    (* Remove program boundaries.  (If this pass isn't active,
       program boundaries are retained even if they're not
       jumped to. *)
    | RemoveBoundaries
    (* Remove elements that have an effect in the assembly, but said
       effect isn't captured in the litmus test. *)
    | RemoveLitmus
    (* Remove elements with no (direct) effect in the assembly. *)
    | RemoveUseless
    (* Simplify elements that aren't directly understandable by
       litmus tools. *)
    | SimplifyLitmus
    (* Warn about things the sanitiser doesn't understand. *)
    | Warn

  (** We include the usual enum extensions for [Pass]. *)
  include Enum.ExtensionTable with type t := t

  (** [explain] collects passes that are useful for explaining an
     assembly file without modifying its semantics too much. *)
  val explain : Set.t
end

(*
 * Context
 *)

(** [CtxIntf] is the interface to the state monad used by the sanitiser to
    carry global information around in a sanitisation pass. *)
module type CtxIntf = sig
  module Lang : Language.Intf
  module Warn : WarnIntf with module L = Lang

  type ctx =
    { progname : string                   (* Name of current program *)
    ; proglen  : int                      (* Length of current program *)
    ; endlabel : string option            (* End label of current program *)
    ; syms     : Language.Symbol.Table.t  (* Symbol table *)
    ; passes   : Pass.Set.t               (* Enabled passes *)
    ; warnings : Warn.t list
    }

  (** [CtxIntf] includes a state monad, [t]. *)
  include State.Intf with type state := ctx

  (** [initial ~passes ~progname ~proglen] opens an initial context
     for the program with the given name, length, and enabled
     passes. *)
  val initial
    :  passes:Pass.Set.t
    -> progname:string
    -> proglen:int
    -> ctx

  (** [pass_enabled pass] is a contextual computation that returns
      [true] provided that [pass] is enabled. *)
  val pass_enabled : Pass.t -> bool t;;

  (** [p |-> f] guards a contextual computation [f] on the
      pass [p]; it won't run unless [p] is in the current context's
      pass set. *)
  val (|->) : Pass.t -> ('a -> 'a t) -> ('a -> 'a t)

  (** [warn_in_ctx ctx w] adds a warning [w] to [ctx], returning
      the new context. *)
  val warn_in_ctx : ctx -> Warn.body -> ctx

  (** [warn w a] adds a warning [w] to the current context, passing
      [a] through. *)
  val warn : Warn.body -> 'a -> 'a t

  (** [add_sym sym sort] is a context computation that adds
      [sym] to the context symbol table with sort [sort], then
      passes [sym] through. *)
  val add_sym
    :  Language.Symbol.t
    -> Language.Symbol.Sort.t
    -> Language.Symbol.t t
  ;;

  (** [syms_with_sorts sortlist] is a context computation that gets
     the set of all symbols in the context's symbol table with sorts
     in [sortlist]. *)
  val syms_with_sorts
    :  Language.Symbol.Sort.t list
    -> Language.Symbol.Set.t t

  (** [make_fresh_label] generates a fresh label with the given prefix
      (in regards to the context's symbol tables), interns it into the
      context, and returns the generated label. *)
  val make_fresh_label : string -> string t

  (** [make_fresh_heap_loc] generates a fresh heap location symbol with
      the given prefix
      (in regards to the context's symbol tables), interns it into the
      context, and returns the generated location symbol. *)
  val make_fresh_heap_loc : string -> string t
end

(** [CtxMake] builds a context monad for the given language. *)
module CtxMake
  : functor (L : Language.Intf)
    -> functor (C : CustomWarnS)
      -> CtxIntf with module Lang = L and module Warn.C = C

(*
 * Language-dependent parts
 *)

(** [LangHookS] is an interface for language-specific hooks into the
    sanitisation process. *)
module type LangHookS = sig
  module L : Language.Intf
  module Ctx : CtxIntf with module Lang = L

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

  module Output : sig
    (** ['a t] is the type of (successful) sanitiser output,
        containing an ['a]. *)
    type 'a t

    val result : 'a t -> 'a

    val warnings : 'a t -> Warn.t list
  end

  (** [sanitise ?passes stms] sanitises a statement list.

      Optional argument 'passes' controls the sanitisation passes
      used; the default is [Pass.all]. *)
  val sanitise
    :  ?passes:Pass.Set.t
    -> statement list
    -> statement list Output.t

  (** [split_and_sanitise ?passes stms] splits a statement list into
     separate litmus programs, then sanitises each, turning it into a
     list of separate program lists with various litmus-unfriendly
     elements removed or simplified.

      Optional argument 'passes' controls the sanitisation passes
     used; the default is [Pass.all]. *)
  val split_and_sanitise
    :  ?passes:Pass.Set.t
    -> statement list
    -> statement list list Output.t
end

(** [Make] implements the assembly sanitiser for a given language. *)
module Make :
  functor (LH : LangHookS)
    -> Intf with type statement = LH.L.Statement.t
