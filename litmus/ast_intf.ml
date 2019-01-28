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

open Core_kernel
open Utils

(** Signature containing just the parts of an act language needed
    for building Litmus file ASTs. *)
module type Basic = sig
  val name : string
  (** [name] is the Herd name of the language. *)

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving compare, eq, sexp]
    include Pretty_printer.S with type t := t

    include Quickcheck.S with type t := t
    (** Constants must come with a quickcheck generator. *)
  end

  (** Abstract type of statements. *)
  module Statement : sig
    type t [@@deriving sexp]
    include Pretty_printer.S with type t := t

    val make_uniform : t list list -> t list list
    (** [make_uniform listings] pads each listing in [listing] to the
        same length. *)
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

(** {2 AST modules} *)

module type S_id = sig
  type t =
    | Local of int * C_identifier.t
    | Global of C_identifier.t
  [@@deriving compare, sexp]
  ;;

  include Stringable.S with type t := t
  (** Litmus identifiers can be converted to and from strings.
      Note that conversion from strings can fail if the C identifier
      parts don't obey C identifier validation. *)

  include Quickcheckable.S with type t := t
  (** We can generate (valid) Litmus identifiers at random for
     quickchecks. *)

  include Comparable.S_plain with type t := t
  (** Litmus identifiers suit various comparable scenarios, such as
     map keys. *)
end
(** Signature of identifier modules. *)

(** The interface for litmus AST modules.

    AST modules contain, effectively, two litmus ASTs: the raw AST that
    can contain any number of possibly ill-formed elements in any order,
    and a 'validated' subset that obeys various invariants. *)
module type S = sig
  module Lang : Basic

  module Id : S_id

  (** Type of basic elements inside a Litmus predicate.

      The distinction between [Pred_elt] and {{!Pred}Pred} mainly
     exists to make conversion to and from other languages, like
     [Blang], easier. *)
  module Pred_elt : sig
    type t =
      | Eq of Id.t * Lang.Constant.t
    [@@deriving sexp, compare, eq]

    include Quickcheck.S with type t := t
    (** Predicate elements come with a quickcheck generator. *)
  end

  module Pred : sig
    type t =
      | Bracket of t
      | Or of t * t
      | And of t * t
      | Elt of Pred_elt.t
    [@@deriving sexp, compare, eq]
    (** Type of Litmus predicates. *)

    val debracket : t -> t
    (** [debracket pred] removes any brackets in [pred]. *)

    val of_blang : Pred_elt.t Blang.t -> t Or_error.t
    (** [of_blang blang] converts [blang], a Blang expression over
       {{!Pred_elt.t}Pred_elt}, to a {{!t}t}.  It may fail if the
       expression contains elements inexpressible in the Litmus
       syntax. *)

    val to_blang : t -> Pred_elt.t Blang.t
    (** [of_blang pred] converts a [pred] to Blang expression over
       {{!Pred_elt.t}Pred_elt}. *)

    include Quickcheck.S with type t := t
    (** Predicates come with a quickcheck generator. *)
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
    type elt = { id : C_identifier.t; value : Lang.Constant.t } [@@deriving sexp]

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
    { language : C_identifier.t
    ; name     : C_identifier.t
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
    type t [@@deriving sexp_of]
    (** The abstract type of a validated litmus AST. *)

    val name     : t -> C_identifier.t
    (** [name test] gets the name of [test]. *)

    val init     : t -> (C_identifier.t, Lang.Constant.t) List.Assoc.t
    (** [init test] gets the initialiser in [test]. *)

    val programs : t -> Lang.Program.t list
    (** [programs test] gets the program listings in [test], in
        left-right or top-bottom order. *)

    val post     : t -> Post.t option
    (** [post test] gets the postcondition of [test], if one
       exists. *)

    (** For pretty-printing, use one of the functors in [Pp]. *)

    val make
      :  ?post:Post.t
      -> name:C_identifier.t
      -> init:((C_identifier.t, Lang.Constant.t) List.Assoc.t)
      -> programs:Lang.Program.t list
      -> unit
      -> t Or_error.t
      (** [make ?post ~name ~init ~programs ()] directly constructs a validated
         AST with the given fields.  It may fail if the result fails
         validation. *)
  end

  val validate : t -> Validated.t Or_error.t
  (** [validate lit] tries to validate a litmus AST.
      It may fail if the input isn't a valid Litmus program. *)
end

(** {2 Conversion} *)

(** Signature of inputs to the [Convert] functor. *)
module type Basic_convert = sig
  module From : S
  (** The Litmus language from which we're converting. *)
  module To : S
  (** The Litmus language to which we're converting. *)

  val constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)

  val program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t
  (** [constant k] tries to convert [k] to the new language. *)
end
