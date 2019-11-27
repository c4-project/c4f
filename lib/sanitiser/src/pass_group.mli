(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [Sanitiser_pass] contains an enumeration [t] of high-level passes that
    can be enabled or disabled. *)

open Core_kernel (* for Blang *)

open Act_common
open Act_utils

type t =
  [ `Escape_symbols
    (** Mangle symbols to ensure litmus tools can lex them. *)
  | `Language_hooks
    (** Run language-specific hooks.

        Said hooks may be further categorised into passes, so enabling
        [Language_hooks] on its own won't enable all language-specific
        passes. *)
  | `Remove_boundaries
    (** Remove program boundaries.

        If this pass isn't active, program boundaries are retained even if
        they're not jumped to. *)
  | `Remove_litmus
    (** Remove elements that have an effect in the assembly, but said effect
        isn't captured in the litmus test. *)
  | `Remove_useless
    (** Remove elements with no (direct) effect in the assembly. *)
  | `Simplify_deref_chains
    (** Replace 'deref chains' with direct movements. This is a fairly
        heavyweight change. *)
  | `Simplify_litmus
    (** Perform relatively minor simplifications on elements that aren't
        directly understandable by litmus tools. *)
  | `Unmangle_symbols
    (** Where possible, replace symbols with their original C identifiers. *)
  | `Warn  (** Warn about things the sanitiser doesn't understand. *) ]
(** [t] enumerates the various high-level sanitisation passes. *)

include Enum_types.Extension_table with type t := t
(** We include the usual enum extensions for [Pass]. *)

val explain : (t, comparator_witness) Set.t
(** [explain] collects passes that are useful for explaining an assembly file
    without modifying its semantics too much. *)

val light : (t, comparator_witness) Set.t
(** [light] collects passes that clean up the assembly, but don't perform
    semantics-changing removals or simplifications. *)

val standard : (t, comparator_witness) Set.t
(** [standard] collects passes that are considered safe for general
    litmusification. *)

(** A [Blang]-based language for selecting sanitiser passes. *)
module Selector : sig
  type elt = t
  (** [elt] is just a renaming of the outermost [t]. *)

  (** Enumerates the various top-level categories of passes. *)
  module Category : sig
    type t = [`Standard | `Explain | `Light] [@@deriving sexp]
  end

  type t =
    [ elt
    | Category.t
    | `Default
      (** Select whichever set is the default for the given act command. *)
    ]
  [@@deriving sexp]
  (** [t] is the base element type of the language. *)

  include Property_types.S with type t := t
  (** This module implements the usual interface for looking up predicate
      documentation. *)

  val eval_b :
       t Blang.t
    -> default:(elt, comparator_witness) Set.t
    -> (elt, comparator_witness) Set.t
  (** [eval_b stm ~default] evaluates a [Blang] statement [stm], mapping
      `Default` to the set [default]. *)
end
