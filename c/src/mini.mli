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

(** A miniature model of a memalloy-witness-style C program.

    Unlike {{!Ast} Ast}, which tries to be a fairly faithful C abstract
    syntax tree, this module describes a tiny subset of C that maps well to
    litmus tests, and does so in a fairly high-level manner.

    One can get a 'mini' C program by {{!Convert} converting} an AST to it
    (which may fail). To get an AST (for printing, etc.), use
    {{!Mini_reify} Reify}. *)

open Core_kernel
open Utils

include module type of Ast_basic

include module type of Mini_intf

(** Re-exporting {{!Mini_type} Type}. *)
module Type = Mini_type

(** Re-exporting {{!Mini_initialiser} Initialiser}. *)
module Initialiser = Mini_initialiser

(** Re-exporting {{!Mini_lvalue} Lvalue}. *)
module Lvalue = Mini_lvalue

(** Re-exporting {{!Mini_address} Address}. *)
module Address = Mini_address

(** Re-exporting {{!Mini_expression} Expression}. *)
module Expression = Mini_expression

(** Re-exporting {{!Atomic_load} Atomic_load} from within
    {{!Expression} Expression}. *)
module Atomic_load = Expression.Atomic_load

(** A non-atomic assignment. *)
module Assign : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make : lvalue:Lvalue.t -> rvalue:Expression.t -> t
  (** [make ~lvalue ~rvalue] constructs an assignment of [rvalue] to
      [lvalue]. *)

  (** {3 Accessors} *)

  val lvalue : t -> Lvalue.t
  (** [lvalue asn] gets [asn]'s destination lvalue. *)

  val rvalue : t -> Expression.t
  (** [rvalue asn] gets [asn]'s source expression (rvalue). *)

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in assignments. *)
  module On_addresses :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in assignments. *)
  module On_lvalues :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Lvalue.t
end

(** An atomic store operation. *)
module Atomic_store : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make : src:Expression.t -> dst:Address.t -> mo:Mem_order.t -> t
  (** [atomic_store ~src ~dst ~mo] constructs an explicit atomic store
      expression with source [src], destination [dst], and memory order
      [mo]. *)

  (** {3 Accessors} *)

  val dst : t -> Address.t
  (** [dst st] gets [st]'s destination address. *)

  val src : t -> Expression.t
  (** [src st] gets [st]'s source expression. *)

  val mo : t -> Mem_order.t
  (** [mo st] gets [st]'s memory order. *)

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in atomic stores. *)
  module On_addresses :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in atomic stores. *)
  module On_lvalues :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Lvalue.t

  (** {3 Generating and quickchecking} *)

  (** [Quickcheck_generic (Src) (Dst)] generates random stores, using [Src]
      to generate source expressions and [Dst] to generate destination
      addresses. It uses [Mem_order]'s store-compatible generator to pick
      random memory orders. *)
  module Quickcheck_generic
      (Src : Quickcheck.S with type t := Expression.t)
      (Dst : Quickcheck.S with type t := Address.t) :
    My_quickcheck.S_with_sexp with type t := t

  (** There isn't a generic quickcheck instance for atomic stores, as we
      can't guarantee type safety in general. *)

  (** [Quickcheck_ints (E)] generates random stores from atomic integers to
      non-atomic integers, using [Src] as the variable typing environment
      for sources and [Dst] as the environment for destinations. *)
  module Quickcheck_ints (Src : Mini_env.S) (Dst : Mini_env.S) :
    My_quickcheck.S_with_sexp with type t := t
end

(** A (strong, explicit) atomic compare-exchange operation. *)
module Atomic_cmpxchg : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make :
       obj:Address.t
    -> expected:Address.t
    -> desired:Expression.t
    -> succ:Mem_order.t
    -> fail:Mem_order.t
    -> t
  (** [make ~obj ~expected ~desired ~succ ~fail] constructs an explicit
      strong compare-exchange with object [obj], expected value store
      [expected], desired final value [desired], and memory orders [succ] on
      success and [fail] on failure. *)

  (** {3 Accessors} *)

  val obj : t -> Address.t
  (** [obj cmpxchg] gets [cmpxchg]'s object address (the main target of the
      operation). *)

  val expected : t -> Address.t
  (** [expected cmpxchg] gets [cmpxchg]'s expected address (the location
      that holds the expected value, and receives the actual value). *)

  val desired : t -> Expression.t
  (** [desired cmpxchg] gets [cmpxchg]'s desired-value expression (written
      to the object on success). *)

  val succ : t -> Mem_order.t
  (** [succ cmpxchg] gets [cmpxchg]'s memory order on success. *)

  val fail : t -> Mem_order.t
  (** [fail cmpxchg] gets [cmpxchg]'s memory order on failure. *)

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in atomic compare-exchanges. *)
  module On_addresses :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in atomic compare-exchanges. *)
  module On_lvalues :
    Travesty.Traversable.S0 with type t := t and type Elt.t = Lvalue.t
end

(** A statement.

    We treat some things that are technically expressions in C as
    statements, for simplicity. *)
module rec Statement :
  (S_statement
  with type address := Address.t
   and type assign := Assign.t
   and type atomic_cmpxchg := Atomic_cmpxchg.t
   and type atomic_store := Atomic_store.t
   and type identifier := Identifier.t
   and type if_stm := If_statement.t
   and type lvalue := Lvalue.t)

and If_statement :
  (S_if_statement
  with type expr := Expression.t
   and type stm := Statement.t
   and type address := Address.t
   and type identifier := Identifier.t
   and type lvalue := Lvalue.t)

(** A function (less its name). *)
module Function : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make :
       parameters:Type.t id_assoc
    -> body_decls:Initialiser.t id_assoc
    -> ?body_stms:Statement.t list
    -> unit
    -> t
  (** [make ~parameters ~body_decls ?body_stms] creates a function with the
      given contents. *)

  (** {3 Accessors} *)

  val parameters : t -> Type.t id_assoc
  (** [parameters func] gets [func]'s parameter list. *)

  val body_decls : t -> Initialiser.t id_assoc
  (** [body_decls func] gets [func]'s in-body variable declarations. *)

  val body_stms : t -> Statement.t list
  (** [body_decls func] gets [func]'s statements. *)

  val cvars : t -> C_identifier.Set.t
  (** [cvars func] extracts a set of C variable names from [func]. *)

  (** {3 Mutators} *)

  val with_body_stms : t -> Statement.t list -> t
  (** [with_body_stms func new_stms] produces a new function by substituting
      [new_stms] for [func]'s body statements. *)

  val map :
       t
    -> parameters:(Type.t id_assoc -> Type.t id_assoc)
    -> body_decls:(Initialiser.t id_assoc -> Initialiser.t id_assoc)
    -> body_stms:(Statement.t list -> Statement.t list)
    -> t
  (** [map func ~parameters ~body_decls ~body_stms] runs the given functions
      over the respective parts of a function. *)

  (** {3 Traversals} *)

  (** [On_decls] allows traversal over all of the declarations inside a
      function. *)
  module On_decls :
    Travesty.Traversable.S0
    with type t := t
     and type Elt.t := Initialiser.t named
end

module Program : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make :
    globals:Initialiser.t id_assoc -> functions:Function.t id_assoc -> t
  (** [make ~globals ~functions] makes a program with global variable
      declarations [globals] and function definitions [functions]. *)

  (** {3 Accessors} *)

  val globals : t -> Initialiser.t id_assoc
  (** [globals program] gets an associative list of each global initialiser
      in [program]. *)

  val functions : t -> Function.t id_assoc
  (** [functions program] gets an associative list of each function in
      [program]. *)

  val cvars : t -> C_identifier.Set.t
  (** [cvars program] extracts a set of C variable names from [program]. *)

  (** {3 Mutators} *)

  val with_functions : t -> Function.t id_assoc -> t
  (** [with_functions prog new_functions] creates a new program by
      substituting [new_functions] for [prog]'s functions. *)

  (** {3 Traversals} *)

  (** [On_decls] allows traversal over all of the declarations inside a
      program. *)
  module On_decls :
    Travesty.Traversable.S0
    with type t = t
     and type Elt.t = Initialiser.t named
end
