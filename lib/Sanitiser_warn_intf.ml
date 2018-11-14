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

(** Sanitiser: warnings: interfaces *)

open Base

(** [Hook] is the signature for language-specific hooks
    into the sanitiser warnings set. *)
module type Hook = sig
  module Lang : Language.S

  include Pretty_printer.S
end

(** [S] is the interface to the warnings emitted by the sanitiser. *)
module type S = sig
  module Hook : Hook

  type elt =
    | Instruction of Hook.Lang.Instruction.t
    | Statement of Hook.Lang.Statement.t
    | Location of Hook.Lang.Location.t
    | Operands of Hook.Lang.Instruction.t
  ;;

  type body =
    (* Something needed an end-of-program label, but there wasn't one. *)
    | MissingEndLabel
    (* This element isn't known to act, and its translation may be wrong. *)
    | UnknownElt of elt
    (* This element seems erroneous, and its translation may be wrong. *)
    | ErroneousElt of elt * Error.t
    (* Looking up the named symbol in the assembly failed, which may
       cause the reported location table to be wrong. *)
    | SymbolRedirFail of Hook.Lang.Symbol.t
    (* Hook for language-specific sanitiser passes to add warnings. *)
    | Custom of Hook.t

  type t =
    { body     : body
    ; progname : string
    }

  include Pretty_printer.S with type t := t
end

module type Sanitiser_warn = sig
  module type Hook = Hook
  module type S = S

  (** [Make_null_hook] is a dummy functor implementing [Warn_hook]. *)
  module Make_null_hook
    : functor (Lang : Language.S)
      -> Hook with module Lang = Lang
  ;;

  (** [Make] produces a warnings module for the given
     language-specific hook. *)
  module Make : functor (H : Hook) -> S with module Hook = H
end
