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
    {{!Mini_reify}Reify}.
*)

open Core_kernel
open Utils
include module type of Ast_basic
include module type of Mini_intf

(** Re-exporting {{!Mini_type}Type}. *)
module Type = Mini_type

(** Re-exporting {{!Mini_initialiser}Initialiser}. *)
module Initialiser = Mini_initialiser

(** Re-exporting {{!Mini_lvalue}Lvalue}. *)
module Lvalue = Mini_lvalue

(** Re-exporting {{!Mini_address}Address}. *)
module Address = Mini_address

(** Re-exporting {{!Mini_expression}Expression}. *)
module Expression = Mini_expression

(** Re-exporting {{!Atomic_load}Atomic_load} from within
   {{!Expression}Expression}. *)
module Atomic_load = Expression.Atomic_load

(** A non-atomic assignment. *)
module Assign : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  (** [make ~lvalue ~rvalue] constructs an assignment of [rvalue] to
      [lvalue]. *)
  val make : lvalue:Lvalue.t -> rvalue:Expression.t -> t

  (** {3 Accessors} *)

  (** [lvalue asn] gets [asn]'s destination lvalue. *)
  val lvalue : t -> Lvalue.t

  (** [rvalue asn] gets [asn]'s source expression (rvalue). *)
  val rvalue : t -> Expression.t

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in assignments. *)
  module On_addresses :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in assignments. *)
  module On_lvalues :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Lvalue.t
end

(** An atomic store operation. *)
module Atomic_store : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  (** [atomic_store ~src ~dst ~mo] constructs an explicit atomic store
      expression with source [src], destination [dst], and memory order
      [mo]. *)
  val make : src:Expression.t -> dst:Address.t -> mo:Mem_order.t -> t

  (** {3 Accessors} *)

  (** [dst st] gets [st]'s destination address. *)
  val dst : t -> Address.t

  (** [src st] gets [st]'s source expression. *)
  val src : t -> Expression.t

  (** [mo st] gets [st]'s memory order. *)
  val mo : t -> Mem_order.t

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in atomic stores. *)
  module On_addresses :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in atomic stores. *)
  module On_lvalues :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Lvalue.t

  (** {3 Generating and quickchecking} *)

  (** [Quickcheck_generic (Src) (Dst)] generates random stores,
      using [Src] to generate source expressions and [Dst] to
      generate destination addresses.  It uses [Mem_order]'s
      store-compatible generator to pick random memory orders. *)
  module Quickcheck_generic
      (Src : Quickcheckable.S with type t := Expression.t)
      (Dst : Quickcheckable.S with type t := Address.t) :
    Quickcheckable.S with type t := t

  (** There isn't a generic quickcheck instance for atomic stores,
      as we can't guarantee type safety in general. *)

  (** [Quickcheck_ints (E)] generates random stores from atomic
        integers to non-atomic integers, using [Src] as the
        variable typing environment for sources and [Dst] as the
        environment for destinations. *)
  module Quickcheck_ints (Src : Mini_env.S) (Dst : Mini_env.S) :
    Quickcheckable.S with type t := t
end

(** A (strong, explicit) atomic compare-exchange operation. *)
module Atomic_cmpxchg : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  (** [make ~obj ~expected ~desired ~succ ~fail] constructs an
        explicit strong compare-exchange with object [obj], expected
        value store [expected], desired final value [desired], and
        memory orders [succ] on success and [fail] on failure. *)
  val make
    :  obj:Address.t
    -> expected:Address.t
    -> desired:Expression.t
    -> succ:Mem_order.t
    -> fail:Mem_order.t
    -> t

  (** {3 Accessors} *)

  (** [obj cmpxchg] gets [cmpxchg]'s object address (the main target
     of the operation). *)
  val obj : t -> Address.t

  (** [expected cmpxchg] gets [cmpxchg]'s expected address (the
      location that holds the expected value, and receives the actual
      value). *)
  val expected : t -> Address.t

  (** [desired cmpxchg] gets [cmpxchg]'s desired-value expression
      (written to the object on success). *)
  val desired : t -> Expression.t

  (** [succ cmpxchg] gets [cmpxchg]'s memory order on success. *)
  val succ : t -> Mem_order.t

  (** [fail cmpxchg] gets [cmpxchg]'s memory order on failure. *)
  val fail : t -> Mem_order.t

  (** {3 Traversals} *)

  (** Traversing over atomic-action addresses in atomic
      compare-exchanges. *)
  module On_addresses :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Address.t

  (** Traversing over lvalues in atomic compare-exchanges. *)
  module On_lvalues :
    Travesty.Traversable.S0_container with type t := t and type Elt.t = Lvalue.t
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

  (** [make ~parameters ~body_decls ?body_stms] creates a function
      with the given contents. *)
  val make
    :  parameters:Type.t id_assoc
    -> body_decls:Initialiser.t id_assoc
    -> ?body_stms:Statement.t list
    -> unit
    -> t

  (** {3 Accessors} *)

  (** [parameters func] gets [func]'s parameter list. *)
  val parameters : t -> Type.t id_assoc

  (** [body_decls func] gets [func]'s in-body variable
      declarations. *)
  val body_decls : t -> Initialiser.t id_assoc

  (** [body_decls func] gets [func]'s statements. *)
  val body_stms : t -> Statement.t list

  (** [cvars func] extracts a set of C variable names from
      [func]. *)
  val cvars : t -> C_identifier.Set.t

  (** {3 Mutators} *)

  (** [with_body_stms func new_stms] produces a new function by
     substituting [new_stms] for [func]'s body statements. *)
  val with_body_stms : t -> Statement.t list -> t

  (** [map func ~parameters ~body_decls ~body_stms] runs the given
      functions over the respective parts of a function. *)
  val map
    :  t
    -> parameters:(Type.t id_assoc -> Type.t id_assoc)
    -> body_decls:(Initialiser.t id_assoc -> Initialiser.t id_assoc)
    -> body_stms:(Statement.t list -> Statement.t list)
    -> t

  (** {3 Traversals} *)

  (** [On_decls] allows traversal over all of the declarations
      inside a function. *)
  module On_decls :
    Travesty.Traversable.S0_container
    with type t := t
     and type Elt.t := Initialiser.t named
end

module Program : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  (** [make ~globals ~functions] makes a program with global variable
      declarations [globals] and function definitions [functions]. *)
  val make : globals:Initialiser.t id_assoc -> functions:Function.t id_assoc -> t

  (** {3 Accessors} *)

  (** [globals program] gets an associative list of each global
     initialiser in [program]. *)
  val globals : t -> Initialiser.t id_assoc

  (** [functions program] gets an associative list of each function in
     [program]. *)
  val functions : t -> Function.t id_assoc

  (** [cvars program] extracts a set of C variable names from
      [program]. *)
  val cvars : t -> C_identifier.Set.t

  (** {3 Mutators} *)

  (** [with_functions prog new_functions] creates a new program by
     substituting [new_functions] for [prog]'s functions. *)
  val with_functions : t -> Function.t id_assoc -> t

  (** {3 Traversals} *)

  (** [On_decls] allows traversal over all of the declarations
      inside a program. *)
  module On_decls :
    Travesty.Traversable.S0_container
    with type t := t
     and type Elt.t := Initialiser.t named
end
