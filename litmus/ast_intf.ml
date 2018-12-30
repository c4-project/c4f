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

open Base

(** Signature containing just the parts of an act language needed
    for building Litmus file ASTs. *)
module type Basic = sig
  val name : string
  (** [name] is the Herd name of the language. *)

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t
  end

  (** Abstract type of statements. *)
  module Statement : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t

    val empty : unit -> t
    (** [empty ()] is a blank statement, used to fill up non-uniform
       Litmus tests when pretty-printing them. *)
  end

  (** Abstract type of programs. *)
  module Program : sig
    type t [@@deriving sexp]

    val name : t -> string option
    (** [name program] gets the declared name of [program], if it has
       one. *)

    val listing : t -> Statement.t list
    (** [listing program] gets [program]'s statement listing. *)

    include Pretty_printer.S with type t := t
  end
end

(** The interface for litmus AST modules.

    AST modules contain, effectively, two litmus ASTs: the raw AST that
    can contain any number of possibly ill-formed elements in any order,
    and a 'validated' subset that obeys various invariants. *)
module type S = sig
  module Lang : Basic

  module Id : sig
    type t =
      | Local of int * string
      | Global of string
    [@@deriving sexp]
    ;;
  end

  module Pred : sig
    type t =
      | Bracket of t
      | Or of t * t
      | And of t * t
      | Eq of Id.t * Lang.Constant.t
    [@@deriving sexp]
    ;;
  end

  module Post : sig
    type t =
      { quantifier : [ `Exists ]
      ; predicate  : Pred.t
      }
    [@@deriving sexp]
    ;;
  end

  module Init : sig
    type elt = { id : string; value : Lang.Constant.t } [@@deriving sexp]

    type t = elt list [@@deriving sexp]
  end

  module Decl : sig
    type t =
      | Program of Lang.Program.t
      | Init    of Init.t
      | Post of Post.t
    [@@deriving sexp]
    ;;
  end

  type t =
    { language : string
    ; name     : string
    ; decls    : Decl.t list
    }
    [@@deriving sexp]
  (** The type of (non-validated) litmus ASTs. *)

  (* TODO(@MattWindsor91): expose constructors *)

  (** Validated litmus ASTs.

      A 'valid' litmus AST is one that has a well-formed name, the
      correct language, exactly one init block, at most one
      postcondition block, and a set of appropriately named programs.
  *)
  module Validated : sig
    type t
    (** The abstract type of a validated litmus AST. *)

    val name     : t -> string
    val init     : t -> (string, Lang.Constant.t) List.Assoc.t
    val programs : t -> Lang.Program.t list

    (** For pretty-printing, use one of the functors in [Pp]. *)

    val make
      :  name:string
      -> init:((string, Lang.Constant.t) List.Assoc.t)
      -> programs:Lang.Program.t list
      -> t Or_error.t
      (** [make ~name ~init ~programs] directly constructs a validated
         AST with the given fields.  It may fail if the result fails
         validation. *)
  end

  val validate : t -> Validated.t Or_error.t
  (** [validate lit] tries to validate a litmus AST.
      It may fail if the input isn't a valid Litmus program. *)
end
