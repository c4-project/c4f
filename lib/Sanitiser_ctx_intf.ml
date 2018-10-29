(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Sanitiser: context state monad: interfaces *)

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

(** [WarnIntf] is the interface to the warnings emitted by the
   sanitiser. *)
module type WarnIntf = sig
  module L : Language.Intf
  module C : CustomWarnS

  type elt =
    | Instruction of L.Instruction.t
    | Statement of L.Statement.t
    | Location of L.Location.t
    | Operands of L.Instruction.t

  type body =
    (* Something needed an end-of-program label, but there wasn't one. *)
    | MissingEndLabel
    (* This element isn't known to act, and its translation may be wrong. *)
    | UnknownElt of elt
    (* This element seems erroneous, and its translation may be wrong. *)
    | ErroneousElt of elt
    (* A symbol redirection failed, which may cause the reported
       location table to be wrong. *)
    | SymbolRedirFail of { src : L.Symbol.t
                         ; dst : L.Symbol.t option
                         ; why : Error.t
                         }
    (* Hook for language-specific sanitiser passes to add warnings. *)
    | Custom of C.t

  type t =
    { body     : body
    ; progname : string
    }

  include Pretty_printer.S with type t := t
end

(*
 * Context
 *)

(** [Intf] is the interface to the state monad used by the sanitiser to
    carry global information around in a sanitisation pass. *)
module type Intf = sig
  module Lang : Language.Intf
  module Warn : WarnIntf with module L = Lang

  type ctx

  (** [Intf] includes a state monad, [t]. *)
  include State.Intf with type state := ctx

  (** [initial ~passes ~progname ~proglen] opens an initial context
     with the given enabled passes. *)
  val initial
    :  passes:Sanitiser_pass.Set.t
    -> ctx

  (*
   * Program properties
   *)

  (** [enter_program ~name ~len] is a contextual computation that
      tells the context we've entered a new program with name
      [name] and body [body].  Any previous program state is
      expunged. *)
  val enter_program
    :  name : string
    -> Lang.Statement.t list
    -> (Lang.Statement.t list) t
  ;;

  (** [get_end_label] is a contextual computation that returns the
      program's current end label. *)
  val get_end_label : string option t

  (** [set_end_label] is a contextual computation that sets the
     program's current end label and increments the program length
     counter, in anticipation of the program gaining a new label
     statement. *)
  val set_end_label : string -> unit t

  (** [get_prog_name] is a contextual computation that returns the
     program's name. *)
  val get_prog_name : string t

  (** [get_prog_length] is a contextual computation that returns the
     program's length. *)
  val get_prog_length : int t

  (** [dec_prog_length] is a contextual computation that decrements
      the program's length. *)
  val dec_prog_length : unit t

  (*
   * Conditional execution
   *)

  (** [is_pass_enabled pass] is a contextual computation that returns
      [true] provided that [pass] is enabled. *)
  val is_pass_enabled : Sanitiser_pass.t -> bool t

  (** [p |-> f] guards a contextual computation [f] on the
      pass [p]; it won't run unless [p] is in the current context's
      pass set. *)
  val (|->) : Sanitiser_pass.t -> ('a -> 'a t) -> ('a -> 'a t)

  (** [warn w a] adds a warning [w] to the current context. *)
  val warn : Warn.body -> unit t

  (** [take_warnings] is a contextual computation that returns the
      current warning set, while clearing it inside the context. *)
  val take_warnings : Warn.t list t

  (*
   * Symbols
   *)

  (** [get_symbol_table] is a contextual computation that returns the
     current symbol table. *)
  val get_symbol_table : Abstract.Symbol.Table.t t

  (** [get_symbols_with_sorts sortlist] is a context computation that
     gets the set of all symbols in the context's symbol table with
     sorts in [sortlist]. *)
  val get_symbols_with_sorts
    :  Abstract.Symbol.Sort.t list
    -> Abstract.Symbol.Set.t t
  ;;

  (** [add_symbol sym sort] is a context computation that adds [sym]
     to the context symbol table with sort [sort], then passes [sym]
     through. *)
  val add_symbol
    :  Abstract.Symbol.t
    -> Abstract.Symbol.Sort.t
    -> Abstract.Symbol.t t
  ;;

  (** [set_symbol_table sym sort] is a context computation that replaces
      the current symbol table with [syms]. *)
  val set_symbol_table
    :  Abstract.Symbol.Table.t
    -> unit t
  ;;

  (** [redirect ~src ~dst] tries to redirect ~src to ~dst in the
     concrete symbol redirection table.  If the redirect causes an
     error, the redirect has no effect and a warning is triggered. *)
  val redirect
    :  src : Lang.Symbol.t
    -> dst : Lang.Symbol.t
    -> unit t
  ;;

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

module type Sanitiser_ctx = sig
  module type WarnIntf = WarnIntf
  module type CustomWarnS = CustomWarnS
  module type Intf = Intf

  (** [NoCustomWarn] is a dummy interface implementing [CustomWarnS]. *)
  module NoCustomWarn : CustomWarnS

  (** [Warn] produces a warnings module for the given language and
      custom warnings set. *)
  module Warn
    : functor (L : Language.Intf)
      -> functor (C : CustomWarnS)
        -> WarnIntf with module L = L and module C = C
  ;;

  (** [Make] builds a context monad for the given language. *)
  module Make
    : functor (L : Language.Intf)
      -> functor (C : CustomWarnS)
        -> Intf with module Lang = L
                 and module Warn.C = C
  ;;
end
