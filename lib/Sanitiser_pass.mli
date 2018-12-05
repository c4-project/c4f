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

(** [Sanitiser_pass] contains an enumeration [t] of high-level passes
   that can be enabled or disabled. *)

open Core_kernel
open Utils

(** [t] enumerates the various high-level sanitisation passes. *)
type t =
  [ `Language_hooks (** Run language-specific hooks.

                        Said hooks may be further categorised into
                       passes, so enabling [Language_hooks] on its own
                       won't enable all language-specific passes. *)
  | `Mangle_symbols (** Mangle symbols to ensure litmus tools can lex
                       them. *)
  | `Remove_boundaries (** Remove program boundaries.

                          If this pass isn't active, program
                          boundaries are retained even if they're not
                          jumped to. *)
  | `Remove_litmus (** Remove elements that have an effect in the
                      assembly, but said effect isn't captured in the
                      litmus test. *)
  | `Remove_useless (** Remove elements with no (direct) effect in the
                       assembly. *)
  | `Simplify_deref_chains (** Replace 'deref chains' with direct
                              movements.  This is a fairly heavyweight
                              change. *)
  | `Simplify_litmus (** Perform relatively minor simplifications on
                        elements that aren't directly understandable
                        by litmus tools. *)
  | `Warn  (** Warn about things the sanitiser doesn't understand. *)
  ]
;;

(** We include the usual enum extensions for [Pass]. *)
include Enum.Extension_table with type t := t

(** [explain] collects passes that are useful for explaining an
    assembly file without modifying its semantics too much. *)
val explain : Set.t

(** [standard] collects passes that are considered safe for
    general litmusification. *)
val standard : Set.t

(** [Select_lang] exposes a [Blang]-based language for selecting
    sanitiser passes. *)
module Selector : sig
  (** [elt] is just a renaming of the outermost [t]. *)
  type elt = t

  (** [category] enumerates the various top-level categories of
     passes. *)
  type category =
    [ `Standard
    | `Explain
    ]
  [@@deriving sexp]
  ;;

  (** [t] is the base element type of the language. *)
  type t =
    [ elt      (** Select an individual item *)
    | category (** Select an entire category *)
    | `Default (** Select whichever set is the default for the given
                   act command *)
    ]
  [@@deriving sexp]
  ;;

  (** [eval_b stm ~default] evaluates a [Blang] statement [stm],
      mapping `Default` to the set [default]. *)
  val eval_b : t Blang.t -> default:Set.t -> Set.t
end
