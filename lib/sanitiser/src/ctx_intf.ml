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

(** Sanitiser: context state monad: signatures *)

open Base

(** [S] is the interface to the state monad used by the sanitiser to carry
    global information around in a sanitisation pass. *)
module type S = sig
  module Lang : Act_language.Definition.S

  module Warn :
    Warn.S with type t = Lang.Element.t Warn.t and type elt = Lang.Element.t

  (** [S] includes a state transformer, [t], with an inner error monad. *)
  include Travesty.State_transform_types.S with module Inner := Or_error

  val initial :
    passes:Pass_group.Set.t -> variables:Lang.Symbol.Set.t -> state
  (** [initial ~passes ~variables] opens an initial context with the given
      enabled passes and C variables. *)

  (*
   * Program properties
   *)

  val enter_program : name:string -> unit t
  (** [enter_program ~name] is a contextual computation that tells the
      context we've entered a new program with name [name]. Any previous
      program state is expunged. *)

  val get_end_label : string option t
  (** [get_end_label] is a contextual computation that returns the program's
      current end label. *)

  val set_end_label : string -> unit t
  (** [set_end_label] is a contextual computation that sets the program's
      current end label and increments the program length counter, in
      anticipation of the program gaining a new label statement. *)

  val get_prog_name : string t
  (** [get_prog_name] is a contextual computation that returns the program's
      name. *)

  (*
   * Conditional execution
   *)

  val is_pass_enabled : Pass_group.t -> bool t
  (** [is_pass_enabled pass] is a contextual computation that returns [true]
      provided that [pass] is enabled. *)

  val guard : ('a -> 'a t) -> on:Pass_group.t -> 'a -> 'a t
  (** [guard f ~on] guards a contextual computation [f] on the pass [on]; it
      won't run unless [on] is in the current context's pass set. *)

  val warn : Warn.elt -> Info.t -> unit t
  (** [warn element body] adds a warning [body] to the current context,
      concerning element [element]. *)

  val warn_if : bool -> Warn.elt -> Info.t -> unit t
  (** [warn_if predicate element body] behaves as [warn element body] if
      [predicate] is true, and [return ()] otherwise. *)

  val take_warnings : string -> Warn.t list t
  (** [take_warnings program_name] is a contextual computation that returns
      the warning set for [program_name], while clearing it inside the
      context. *)

  (*
   * Symbols
   *)

  val get_symbol_table : Act_abstract.Symbol.Table.t t
  (** [get_symbol_table] is a contextual computation that returns the
      current symbol table. *)

  val get_symbols_with_sorts :
    Act_abstract.Symbol.Sort.t list -> Act_abstract.Symbol.Set.t t
  (** [get_symbols_with_sorts sortlist] is a context computation that gets
      the set of all symbols in the context's symbol table with sorts in
      [sortlist]. *)

  val add_symbol :
       Act_abstract.Symbol.t
    -> Act_abstract.Symbol.Sort.t
    -> Act_abstract.Symbol.t t
  (** [add_symbol sym sort] is a context computation that adds [sym] to the
      context symbol table with sort [sort], then passes [sym] through. *)

  val set_symbol_table : Act_abstract.Symbol.Table.t -> unit t
  (** [set_symbol_table sym sort] is a context computation that replaces the
      current symbol table with [syms]. *)

  val get_variables : Lang.Symbol.Set.t t
  (** [get_variables] looks up the original set of C variables passed into
      this sanitiser context on creation. *)

  val get_redirect : Lang.Symbol.t -> Lang.Symbol.t t
  (** [get_redirect sym] looks up [sym] in the concrete symbol redirection
      table. If [sym] redirects to itself, the result is [sym]. *)

  val get_redirect_sources : Lang.Symbol.t -> Lang.Symbol.Set.t t
  (** [get_redirect_sources sym] returns all symbols that redirect to [sym]
      in the currnet context. *)

  val get_redirect_alist :
    Lang.Symbol.t list -> (Lang.Symbol.t, Lang.Symbol.t) List.Assoc.t t
  (** [get_redirect_alist syms] looks up each symbol in [syms] in the
      concrete symbol redirection table, and builds an associative list from
      each such symbol that has a redirection. *)

  val get_all_redirect_targets : Lang.Symbol.Set.t t
  (** [get_all_redirect_targets] gets a list of all symbols currently set as
      the target of a redirect. *)

  val modify_rmap :
    f:(Lang.Symbol.R_map.t -> Lang.Symbol.R_map.t Or_error.t) -> unit t
  (** [modify_rmap ~f] applies [f] to the context's redirect map. *)

  val redirect : src:Lang.Symbol.t -> dst:Lang.Symbol.t -> unit t
  (** [redirect ~src ~dst] tries to redirect ~src to ~dst in the concrete
      symbol redirection table. If the redirect causes an error, the
      redirect has no effect and a warning is triggered. *)

  val make_fresh_label : string -> string t
  (** [make_fresh_label] generates a fresh label with the given prefix (in
      regards to the context's symbol tables), interns it into the context,
      and returns the generated label. *)

  val make_fresh_heap_loc : string -> string t
  (** [make_fresh_heap_loc] generates a fresh heap location symbol with the
      given prefix (in regards to the context's symbol tables), interns it
      into the context, and returns the generated location symbol. *)
end
