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

(** Sanitiser: output record. *)

module Program : sig
  (** Opaque (for now). *)
  type ('warn_elt, 'listing) t

    val listing : (_, 'listing) t -> 'listing
    (** [listing p] gets the final sanitised program listing. *)

    val warnings : ('warn_elt, _) t -> 'warn_elt Warn.t list
    (** [warnings t] gets the warning list for this program. *)

    val symbol_table : (_, _) t -> Act_abstract.Symbol.Table.t
    (** [symbol_table t] gets the program's final symbol table. *)

    val make :
         ?warnings:'warn_elt Warn.t list
      -> listing:'listing
      -> symbol_table:Act_abstract.Symbol.Table.t
      -> unit
      -> ('warn_elt, 'listing) t
    (** [make ?warnings ~listing ~symbol_table] makes a program output with
        the warnings set [warnings], program listing [listing], and final
        symbol table [symbol_table]. *)

  val strip_listing : ('warn_elt, _) t -> ('warn_elt, unit) t
end

type ('warn_elt, 'listing, 'rmap) t

val programs : ('warn_elt, 'listing, _) t -> ('warn_elt, 'listing) Program.t list
(** [programs t] gets the final sanitised programs. *)

val redirects : (_, _, 'rmap) t -> 'rmap
(** [redirects t] gets the final mapping from C-level symbols to the
    symbols in [programs t]. *)

val make : ?programs:('warn_elt, 'listing) Program.t list -> redirects:'rmap -> unit -> ('warn_elt, 'listing, 'rmap) t
(** [make ?programs ~redirects ()] makes an output using the given list of
    programs [programs] and redirects map [redirects]. *)

val map_programs : ('w1, 'l1, 'rmap) t ->
    f:(('w1, 'l1) Program.t -> ('w2, 'l2) Program.t)
    -> ('w2, 'l2, 'rmap) t
(** [map_programs output ~f] maps [f] over every program in [output]. *)

val map_redirects : ('w, 'l, 'r1) t ->
    f:('r1 -> 'r2)
  -> ('w, 'l, 'r2) t
(** [map_redirects output ~f] maps [f] over the redirects table in [output]. *)
