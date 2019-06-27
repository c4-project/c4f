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

(** Mini-model: expressions *)

(** An atomic load operation. *)
module Atomic_load : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make : src:Mini_address.t -> mo:Mem_order.t -> t
  (** [atomic_load ~src ~dst ~mo] constructs an explicit atomic load
      expression with source [src] and memory order [mo]. *)

  (** {3 Accessors} *)

  val src : t -> Mini_address.t
  (** [src ld] gets [ld]'s source address. *)

  val mo : t -> Mem_order.t
  (** [mo ld] gets [ld]'s memory order. *)

  include
    Mini_intf.S_has_underlying_variable with type t := t
  (** We can get to the variable name inside an atomic load (that is, the
      source variable). *)

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in atomic loads. *)
  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mini_address.t

  (** Traversing over lvalues in atomic loads. *)
  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mini_lvalue.t
end

type t [@@deriving sexp]
(** Opaque type of expressions. *)

(** {2 Constructors} *)

val atomic_load : Atomic_load.t -> t
(** [atomic_load a] lifts an atomic load [a] to an expression. *)

val bool_lit : bool -> t
(** [bool_lit b] lifts a Boolean literal [b] to an expression. *)

val constant : Act_c_lang.Ast_basic.Constant.t -> t
(** [constant k] lifts a C constant [k] to an expression. *)

val eq : t -> t -> t
(** [eq l r] generates an equality expression. *)

val lvalue : Mini_lvalue.t -> t
(** [lvalue lv] lifts a lvalue [lv] to an expression. *)

(** {2 Accessors} *)

val reduce :
     t
  -> bool_lit:(bool -> 'a)
  -> constant:(Act_c_lang.Ast_basic.Constant.t -> 'a)
  -> lvalue:(Mini_lvalue.t -> 'a)
  -> atomic_load:(Atomic_load.t -> 'a)
  -> eq:('a -> 'a -> 'a)
  -> 'a
(** [reduce expr ~bool_lit ~constant ~lvalue ~atomic_load ~eq] recursively
    reduces [expr] to a single value, using the given functions at each
    corresponding stage of the expression tree. *)

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in expressions. *)
module On_addresses :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Mini_address.t

(** Traversing over identifiers in expressions. *)
module On_identifiers :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Act_c_lang.Ast_basic.Identifier.t

(** Traversing over lvalues in expressions. *)
module On_lvalues :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Mini_lvalue.t

(** {2 Generation and quickchecking} *)

module Quickcheck_int_values (E : Mini_env.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random, type-safe expressions over the given variable typing
    environment, with type 'int'. *)

module Quickcheck_bool_values (E : Mini_env.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random, type-safe expressions over the given variable typing
    environment, with type 'bool'. *)

(** {2 Type checking} *)

include
  Mini_intf.S_type_checkable with type t := t
(** Type-checking for expressions. *)
