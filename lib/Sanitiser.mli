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

(*
 * Language-dependent parts
 *)

(** [LangHookS] is an interface for language-specific hooks into the
    sanitisation process. *)
module type LangHookS = sig
  module L : Language.Intf
  module Ctx : Sanitiser_ctx.Intf with module Lang = L

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
  module Warn : Sanitiser_ctx.WarnIntf

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
    :  ?passes:Sanitiser_pass.Set.t
    -> statement list
    -> statement list Output.t

  (** [split_and_sanitise ?passes stms] splits a statement list into
     separate litmus programs, then sanitises each, turning it into a
     list of separate program lists with various litmus-unfriendly
     elements removed or simplified.

      Optional argument 'passes' controls the sanitisation passes
     used; the default is [Pass.all]. *)
  val split_and_sanitise
    :  ?passes:Sanitiser_pass.Set.t
    -> statement list
    -> statement list list Output.t
end

(** [Make] implements the assembly sanitiser for a given language. *)
module Make :
  functor (LH : LangHookS)
    -> Intf with type statement = LH.L.Statement.t
