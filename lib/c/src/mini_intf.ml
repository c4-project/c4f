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

(** Mini-model: module signatures and basic types *)

open Core_kernel

(** Shorthand for pairs of items and their names. *)
type 'a named = Act_common.C_id.t * 'a [@@deriving equal, sexp]

(** Shorthand for associative lists with identifier keys. *)
type 'a id_assoc = (Act_common.C_id.t, 'a) List.Assoc.t [@@deriving sexp]

(** {2 General signatures} *)

(** Signature of modules that expose a 'named' part of a mini-model element,
    usually for compatibility with functors. *)
module type S_named = sig
  type elt

  type t = elt named [@@deriving eq]
end

(** Signature of abstract data types that wrap some C variable name. *)
module type S_has_underlying_variable = sig
  (** The type that contains underlying variables. *)
  type t

  val variable_of : t -> Act_common.C_id.t
  (** [variable_of x] is the underlying variable of [x]. *)

  val variable_in_env : t -> env:_ Act_common.C_id.Map.t -> bool
  (** [variable_in_env x ~env] gets whether [x]'s underlying variable name
      is a key in an environment [env]. *)
end

(** Signature of parts of the mini-model that implement type checking. *)
module type S_type_checkable = sig
  (** The type being checked. *)
  type t

  module Type_check (E : Mini_env.S) : sig
    val type_of : t -> Mini_type.t Or_error.t
    (** [type_of x] tries to get the type of [x] given the variable typing
        environment [E.env]. It fails if the type is inconsistent. *)
  end
end

(** {2 Signatures for recursive modules} *)

module type S_address_traversable = sig
  type t

  type address

  (** Traversing over atomic-action addresses. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = address
end

module type S_lvalue_traversable = sig
  type t

  type lvalue

  (** Traversing over lvalues. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = lvalue
end

module type S_identifier_traversable = sig
  type t

  type identifier

  (** Traversing over identifiers. *)
  module On_identifiers :
    Travesty.Traversable_types.S0
    with type t := t
     and type Elt.t = identifier
end

module type S_statement = sig
  type t [@@deriving sexp]

  type assign

  type atomic_cmpxchg

  type atomic_store

  type if_stm

  (** {3 Constructors} *)

  val assign : assign -> t
  (** [assign a] lifts an assignment [a] to a statement. *)

  val atomic_cmpxchg : atomic_cmpxchg -> t
  (** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to a
      statement. *)

  val atomic_store : atomic_store -> t
  (** [atomic_store a] lifts an atomic store [a] to a statement. *)

  val if_stm : if_stm -> t
  (** [if_statement ifs] lifts an if statement [ifs] to a statement. *)

  val nop : unit -> t
  (** [nop] is a no-operation statement; it corresponds to C's empty
      expression statement. *)

  val map :
       t
    -> assign:(assign -> 'result)
    -> atomic_cmpxchg:(atomic_cmpxchg -> 'result)
    -> atomic_store:(atomic_store -> 'result)
    -> if_stm:(if_stm -> 'result)
    -> nop:(unit -> 'result)
    -> 'result
  (** [map stm ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop] maps the
      appropriate function of those given over [stm]. It does _not_
      recursively reduce statements inside blocks. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         t
      -> assign:(assign -> assign M.t)
      -> atomic_cmpxchg:(atomic_cmpxchg -> atomic_cmpxchg M.t)
      -> atomic_store:(atomic_store -> atomic_store M.t)
      -> if_stm:(if_stm -> if_stm M.t)
      -> nop:(unit -> unit M.t)
      -> t M.t
  end

  include S_address_traversable with type t := t

  include S_lvalue_traversable with type t := t

  include S_identifier_traversable with type t := t
end

module type S_if_statement = sig
  type expr

  type stm

  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make :
    cond:expr -> ?t_branch:stm list -> ?f_branch:stm list -> unit -> t
  (** [if_stm ~cond ?t_branch ?f_branch ()] creates an if statement with
      condition [cond], optional true branch [t_branch], and optional false
      branch [f_branch]. *)

  (** {3 Accessors} *)

  val cond : t -> expr
  (** [cond ifs] gets [ifs]'s condition. *)

  val t_branch : t -> stm list
  (** [t_branch ifs] gets [ifs]'s true branch. *)

  val f_branch : t -> stm list
  (** [f_branch ifs] gets [ifs]'s false branch. *)

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         t
      -> cond:(expr -> expr M.t)
      -> t_branch:(stm list -> stm list M.t)
      -> f_branch:(stm list -> stm list M.t)
      -> t M.t
  end

  include S_address_traversable with type t := t

  include S_lvalue_traversable with type t := t

  include S_identifier_traversable with type t := t
end
