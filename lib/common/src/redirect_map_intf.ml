(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Signatures used in redirect maps. *)

open Base
open Act_utils

module type Basic_symbol = sig
  type t [@@deriving sexp, equal]

  include Core_kernel.Comparable.S with type t := t

  include Stringable.S with type t := t

  val of_c_identifier : C_identifier.t -> t Or_error.t

  val to_c_identifier : t -> C_identifier.t Or_error.t
end

module type S = sig
  (** Type of symbols. *)
  type sym

  (** Type of symbol sets. *)
  type sym_set

  (** Opaque type of redirect maps. *)
  type t [@@deriving sexp_of]

  (** {3 Constructors} *)

  val of_symbol_alist : (sym, sym) List.Assoc.t -> t Or_error.t
  (** [of_symbol_alist alist] tries to lift [alist] into a redirect map. It
      fails if there are duplicate keys. *)

  val identity : unit -> t
  (** [identity s] creates an empty mapping. *)

  (** {3 Mutators} *)

  val redirect : src:sym -> dst:sym -> t -> t
  (** [redirect ~src ~dst rmap] marks [src] as having redirected to [dst] in
      [rmap]. It is safe for [src] and [dst] to be equal; this clears the
      redirection. *)

  val to_string_alist : t -> (string, string) List.Assoc.t
  (** [to_string_alist map] converts [map] into a string-to-string
      associative list. It fails if [map]'s symbols are inexpressible as C
      identifiers. *)

  (** {3 Looking up symbols} *)

  val dest_of_sym : t -> sym -> sym
  (** [dest_of_sym map sym] tries to look up a symbol [sym] in a redirect
      map [map]. It returns [sym] if [sym] has no redirection. *)

  val dest_syms : t -> sources:sym_set -> sym_set
  (** [dest_syms map ~sources] collects all of the destination symbols in
      [map] that are reachable from the source symbols [sources]. This is a
      useful approximation as to which symbols are heap references. *)

  val sources_of_sym : t -> sym -> sym_set
  (** [sources_of_sym rmap dst] gives all of the symbols that map to [dst]
      in [rmap]. *)

  (** {3 Looking up C identifiers} *)

  val dest_of_id : t -> C_identifier.t -> C_identifier.t Or_error.t
  (** [dest_of_id map cid] tries to look up a C identifier [cid] in a
      redirect map [map]. It fails if the redirected symbol isn't a valid C
      identifier. *)

  val dest_ids :
    t -> sources:C_identifier.Set.t -> C_identifier.Set.t Or_error.t
  (** [dest_ids map ~sources] gets the set of C identifiers that are
      reachable, in [map], from identifiers in [sources]. It can fail if any
      of the symbols aren't expressible as identifiers. *)

  val transform_c_variables :
    t -> C_variables.Map.t -> C_variables.Map.t Or_error.t
  (** [transform_c_variables map cvars] tries to apply the redirects in
      [map] to [cvars].

      It fails if the resulting map would have duplicate keys, or if [cvars]
      has any variables with thread IDs (these should be flattened first). *)
end
