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

(** A miniature model of a memalloy-witness-style C program.

    Unlike {{!Ast}Ast}, which tries to be a fairly faithful C abstract
    syntax tree, this module describes a tiny subset of C that
    maps well to litmus tests, and does so in a fairly high-level
    manner.

    One can get a 'mini' C program by {{!Convert}converting} an AST
    to it (which may fail).  To get an AST (for printing, etc.), use
    {{!Reify}Reify}.
 *)

open Base

include module type of Ast_basic

type 'a id_assoc = (Identifier.t, 'a) List.Assoc.t
(** Shorthand for associative lists with identifier keys. *)

module Type : sig
  type t

  val int : t
  (** [int] is the int type. *)

  val atomic_int : t
  (** [atomic_int] is the atomic_int type. *)
end

module Initialiser : sig
  type t

  val make : ty:Type.t -> ?value:Constant.t -> unit -> t
  (** [make ~ty ?value ()] makes an initialiser with type [ty] and
      optional initialised value [value]. *)
end

(** Somewhere assignable (a variable, or dereference thereof). *)
module Lvalue : sig
  type t [@@deriving sexp]

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Identifier.t
  (** Traversing over identifiers in lvalues. *)

  val underlying_variable : t -> Identifier.t
  (** [underlying_variable t] gets the underlying variable name of
     [t]. *)
end

(** An address (a lvalue, or reference thereto). *)
module Address : sig
  type t [@@deriving sexp]

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Lvalue.t
  (** Traversing over lvalues in addresses. *)

  val ref : t -> t
  (** [ref t] constructs a &-reference to [t]. *)

  val underlying_variable : t -> Identifier.t
  (** [underlying_variable t] gets the underlying variable name of
     [t]. *)
end

module Expression : sig
  type t [@@deriving sexp]

  module On_addresses
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Address.t
  (** Traversing over atomic-action addresses in expressions. *)

  module On_identifiers
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Identifier.t
  (** Traversing over identifiers in expressions. *)

  module On_lvalues
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Lvalue.t
  (** Traversing over lvalues in expressions. *)

  val constant : Constant.t -> t
  (** [constant k] lifts a C constant [k] to an expression. *)
end

(** A statement.

    We treat some things that are technically expressions in C as
    statements, for simplicity. *)
module Statement : sig
  type t [@@deriving sexp]

  module On_addresses
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Address.t
  (** Traversing over atomic-action addresses in statements. *)

  module On_identifiers
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Identifier.t
  (** Traversing over identifiers in statements. *)

  module On_lvalues
    : Travesty.Traversable.S0_container
        with type t := t and type Elt.t = Lvalue.t
  (** Traversing over lvalues in statements. *)

  val assign : lvalue:Lvalue.t -> rvalue:Expression.t -> t
  (** [assign ~lvalue ~rvalue] lifts a C assignment to a statement. *)
end

module Function : sig
  type t [@@deriving sexp]

  val body_decls : t -> Initialiser.t id_assoc
    (** [body_decls func] gets [func]'s in-body variable
       declarations. *)

  val map
    :  t
    -> parameters:(Type.t id_assoc -> Type.t id_assoc)
    -> body_decls:(Initialiser.t id_assoc -> Initialiser.t id_assoc)
    -> body_stms:(Statement.t list -> Statement.t list)
    -> t
    (** [map func ~parameters ~body_decls ~body_stms] runs the given
        functions over the respective parts of a function. *)
end

module Program : sig
  type t [@@deriving sexp]

  val make
   :  globals:(Initialiser.t id_assoc)
   -> functions:(Function.t id_assoc)
   -> t
   (** [make ~globals ~functions] makes a program with global variable
       declarations [globals] and function definitions [functions]. *)
end

(** Functions for reifying a mini-model into an AST. *)
module Reify : sig
  val func : Identifier.t -> Function.t -> Ast.External_decl.t

  val program : Program.t -> Ast.Translation_unit.t
end

(** The mini-model, packaged up as a Litmus language.

    This language uses {{!Reify}Reify} for all of its pretty-printing
    needs. *)
module Litmus_lang : Litmus.Ast.Basic
  with type Statement.t =
         [ `Stm of Statement.t
         | `Decl of (Identifier.t * Initialiser.t)
         ]
   and type Program.t = (Identifier.t * Function.t)
   and type Constant.t = Constant.t
;;

(** The mini-model's full Litmus AST module. *)
module Litmus_ast : sig
  include Litmus.Ast.S with module Lang := Litmus_lang
  include Base.Pretty_printer.S with type t := Validated.t
end


(** Converting an AST into the mini-model *)
module Convert : sig
  val func
    :  Ast.Function_def.t
    -> (Identifier.t * Function.t) Or_error.t
  (** [func ast] tries to interpret a C function definition AST
      as a mini-model function. *)

  val translation_unit : Ast.Translation_unit.t -> Program.t Or_error.t
  (** [translation_unit ast] tries to interpret a C translation unit AST
      as a mini-model program. *)

  val litmus : Ast.Litmus.Validated.t -> Litmus_ast.Validated.t Or_error.t
  (** [litmus test] tries to interpret a Litmus test over the full C AST
      as one over the mini-model. *)
end
