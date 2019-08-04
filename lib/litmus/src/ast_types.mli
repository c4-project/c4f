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
open Base
module Id = Act_common.Litmus_id

(** {2 Signatures} *)

(** Signature containing just the parts of an act language needed for
    building Litmus file ASTs. *)
module type Basic = sig
  val name : string
  (** [name] is the Herd name of the language. *)

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving compare, eq, sexp, quickcheck]

    include Pretty_printer.S with type t := t
  end

  (** Abstract type of statements. *)
  module Statement : sig
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    val make_uniform : t list list -> t list list
    (** [make_uniform listings] pads each listing in [listing] to the same
        length. *)
  end

  (** Abstract type of types. *)
  module Type : sig
    type t [@@deriving compare, eq, sexp]
  end

  (** Abstract type of programs. *)
  module Program : sig
    type t [@@deriving sexp]

    val name : t -> string option
    (** [name program] gets the declared name of [program], if it has one. *)

    val listing : t -> Statement.t list
    (** [listing program] gets [program]'s statement listing. *)

    val global_vars : t -> Type.t Act_common.C_id.Map.t option
    (** [global_vars program] gets the set of global variables referenced by
        [program], if this makes sense for this particular litmus language. *)

    include Pretty_printer.S with type t := t
  end
end

(** {2 AST modules} *)

(** The interface for litmus AST modules.

    AST modules contain, effectively, two litmus ASTs: the raw AST that can
    contain any number of possibly ill-formed elements in any order, and a
    'validated' subset that obeys various invariants. *)
module type S = sig
  module Lang : Basic

  module Init : sig
    type elt = {id: Act_common.C_id.t; value: Lang.Constant.t}
    [@@deriving sexp]

    type t = elt list [@@deriving sexp]
  end

  module Decl : sig
    type t =
      | Program of Lang.Program.t
      | Init of Init.t
      | Post of Lang.Constant.t Postcondition.t
      | Locations of Act_common.C_id.t list
    [@@deriving sexp]

    val as_program : t -> Lang.Program.t option
    (** [as_program decl] returns [Some p] if [decl] is a program [p], and
        [None] otherwise. *)

    val as_init : t -> Init.t option
    (** [as_init decl] returns [Some i] if [decl] is an init-block [i], and
        [None] otherwise. *)

    val as_post : t -> Lang.Constant.t Postcondition.t option
    (** [as_post decl] returns [Some c] if [decl] is a postcondition [c],
        and [None] otherwise. *)

    val as_locations : t -> Act_common.C_id.t list option
    (** [as_post decl] returns [Some ls] if [decl] is a location list [ls],
        and [None] otherwise. *)
  end

  type t = {language: Act_common.C_id.t; name: string; decls: Decl.t list}
  [@@deriving sexp]
  (** The type of (non-validated) litmus ASTs. *)

  (* TODO(@MattWindsor91): expose constructors *)

  (** Validated litmus ASTs.

      A 'valid' litmus AST is one that has a well-formed name, the correct
      language, exactly one init block, at most one postcondition block, and
      a set of appropriately named programs. *)
  module Validated : sig
    type t [@@deriving sexp_of]
    (** The abstract type of a validated litmus AST. *)

    val name : t -> string
    (** [name test] gets the name of [test]. *)

    val aux : t -> Lang.Constant.t Aux.t
    (** [aux test] gets [test]'s auxiliary data directly. *)

    val init : t -> (Act_common.C_id.t, Lang.Constant.t) List.Assoc.t
    (** [init test] gets the initialiser in [test]. *)

    val programs : t -> Lang.Program.t list
    (** [programs test] gets the program listings in [test], in left-right
        or top-bottom order. *)

    val locations : t -> Act_common.C_id.t list option
    (** [locations test] gets the locations stanza for [test], if it exists. *)

    val postcondition : t -> Lang.Constant.t Postcondition.t option
    (** [postcondition test] gets the postcondition of [test], if one
        exists. *)

    (** For pretty-printing, use one of the functors in [Pp]. *)

    val make :
         name:string
      -> aux:Lang.Constant.t Aux.t
      -> programs:Lang.Program.t list
      -> t Or_error.t
    (** [make ~name ~aux ~programs] directly constructs a validated AST with
        the given fields. It may fail if the result fails validation. *)
  end

  val validate : t -> Validated.t Or_error.t
  (** [validate lit] tries to validate a litmus AST. It may fail if the
      input isn't a valid Litmus program. *)
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
