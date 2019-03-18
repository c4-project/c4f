(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** {2 Signatures} *)

(** Signature containing just the parts of an act language needed
    for building Litmus file ASTs. *)
module type Basic = sig
  (** [name] is the Herd name of the language. *)
  val name : string

  (** Abstract type of constant syntax *)
  module Constant : sig
    type t [@@deriving compare, eq, sexp, quickcheck]

    include Pretty_printer.S with type t := t
  end

  (** Abstract type of statements. *)
  module Statement : sig
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    (** [make_uniform listings] pads each listing in [listing] to the
        same length. *)
    val make_uniform : t list list -> t list list
  end

  (** Abstract type of types. *)
  module Type : sig
    type t [@@deriving compare, eq, sexp]
  end

  (** Abstract type of programs. *)
  module Program : sig
    type t [@@deriving sexp]

    (** [name program] gets the declared name of [program], if it has
       one. *)
    val name : t -> string option

    (** [listing program] gets [program]'s statement listing. *)
    val listing : t -> Statement.t list

    (** [global_vars program] gets the set of global variables referenced
        by [program], if this makes sense for this particular litmus
        language. *)
    val global_vars : t -> Type.t C_identifier.Map.t option

    include Pretty_printer.S with type t := t
  end
end

(** {2 AST modules} *)

(** The interface for litmus AST modules.

    AST modules contain, effectively, two litmus ASTs: the raw AST that
    can contain any number of possibly ill-formed elements in any order,
    and a 'validated' subset that obeys various invariants. *)
module type S = sig
  module Lang : Basic

  (** Convenience re-export of {{!Ast_base.Id}Id}. *)
  module Id : module type of Ast_base.Id

  module Pred_elt : sig
    type t = Lang.Constant.t Ast_base.Pred_elt.t
    [@@deriving compare, sexp, equal, quickcheck]

    include
      Ast_base_intf.S_pred_elt
      with type 'const t := t
       and type 'const elt := Lang.Constant.t
       and type id := Id.t
  end

  module Pred : sig
    type t = Lang.Constant.t Ast_base.Pred.t
    [@@deriving compare, sexp, equal, quickcheck]

    include
      Ast_base_intf.S_pred with type 'const t := t and type 'const elt := Pred_elt.t

    (** [of_blang blang] converts [blang], a Blang expression over
       {{!Pred_elt.t}Pred_elt}, to a {{!t}t}.  It may fail if the
       expression contains elements inexpressible in the Litmus
       syntax. *)
    val of_blang : Pred_elt.t Blang.t -> t Or_error.t

    (** [of_blang pred] converts a [pred] to Blang expression over
       {{!Pred_elt.t}Pred_elt}. *)
    val to_blang : t -> Pred_elt.t Blang.t
  end

  module Postcondition : sig
    type t = Lang.Constant.t Ast_base.Postcondition.t
    [@@deriving sexp, compare, equal, quickcheck]

    include
      Ast_base_intf.S_postcondition
      with type 'const t := t
       and type 'const pred := Pred.t
  end

  module Init : sig
    type elt =
      { id : C_identifier.t
      ; value : Lang.Constant.t
      }
    [@@deriving sexp]

    type t = elt list [@@deriving sexp]
  end

  module Decl : sig
    type t =
      | Program of Lang.Program.t
      | Init of Init.t
      | Post of Postcondition.t
      | Locations of C_identifier.t list
    [@@deriving sexp]

    (** [as_program decl] returns [Some p] if [decl] is a program [p], and [None] otherwise. *)
    val as_program : t -> Lang.Program.t option

    (** [as_init decl] returns [Some i] if [decl] is an init-block [i], and [None] otherwise. *)
    val as_init : t -> Init.t option

    (** [as_post decl] returns [Some c] if [decl] is a postcondition [c], and [None] otherwise. *)
    val as_post : t -> Postcondition.t option

    (** [as_post decl] returns [Some ls] if [decl] is a location list [ls], and [None] otherwise. *)
    val as_locations : t -> C_identifier.t list option
  end

  (** The type of (non-validated) litmus ASTs. *)
  type t =
    { language : C_identifier.t
    ; name : string
    ; decls : Decl.t list
    }
  [@@deriving sexp]

  (* TODO(@MattWindsor91): expose constructors *)

  (** Validated litmus ASTs.

      A 'valid' litmus AST is one that has a well-formed name, the
      correct language, exactly one init block, at most one
      postcondition block, and a set of appropriately named programs.
  *)
  module Validated : sig
    (** The abstract type of a validated litmus AST. *)
    type t [@@deriving sexp_of]

    (** [name test] gets the name of [test]. *)
    val name : t -> string

    (** [init test] gets the initialiser in [test]. *)
    val init : t -> (C_identifier.t, Lang.Constant.t) List.Assoc.t

    (** [programs test] gets the program listings in [test], in
        left-right or top-bottom order. *)
    val programs : t -> Lang.Program.t list

    (** [locations test] gets the locations stanza for [test], if it exists. *)
    val locations : t -> C_identifier.t list option

    (** [postcondition test] gets the postcondition of [test], if one
       exists. *)
    val postcondition : t -> Postcondition.t option

    (** For pretty-printing, use one of the functors in [Pp]. *)

    (** [make ?location ?post ~name ~init ~programs ()] directly
         constructs a validated AST with the given fields.  It may
         fail if the result fails validation. *)
    val make
      :  ?locations:C_identifier.t list
      -> ?postcondition:Postcondition.t
      -> name:string
      -> init:(C_identifier.t, Lang.Constant.t) List.Assoc.t
      -> programs:Lang.Program.t list
      -> unit
      -> t Or_error.t
  end

  (** [validate lit] tries to validate a litmus AST.
      It may fail if the input isn't a valid Litmus program. *)
  val validate : t -> Validated.t Or_error.t
end

(** {2 Conversion} *)

(** Signature of inputs to the [Convert] functor. *)
module type Basic_convert = sig
  (** The Litmus language from which we're converting. *)
  module From : S

  (** The Litmus language to which we're converting. *)
  module To : S

  (** [constant k] tries to convert [k] to the new language. *)
  val constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t

  (** [constant k] tries to convert [k] to the new language. *)
  val program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t
end
