(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: blocks with control flow.

    This module abstracts over anything that contains precisely one block
    with an additional 'header' specifying how control flows into and out of
    it. Examples include:

    - for loops;
    - while loops;
    - locks;
    - transactions. *)

open Base
open Import

(** {1 Flow block headers} *)

(** {2 For loops}

    FIR presently supports for loops on two separate levels:

    - the standard C-style 'init, compare, update' loop, which is useful for
      preserving information about existing loops;
    - a 'simplified' loop whereby a single variable is incremented, or
      decreemented, towards a target.

    This for loop provision will likely expand and change as we target more
    languages, but most actions will target the simplified for loop natively. *)
module For : sig
  (** {3 Simplified loops} *)
  module Simple : sig
    module Inclusivity : sig
      (** Type of inclusivities of simplified for loops. *)
      type t = Exclusive | Inclusive
      [@@deriving sexp, compare, equal, quickcheck]
    end

    module Direction : sig
      (** Type of directions of simplified for loops. *)
      type t = Down of Inclusivity.t | Up of Inclusivity.t
      [@@deriving sexp, compare, equal, quickcheck]
    end

    (** Type of simplified for headers. *)
    type t =
      { lvalue: Lvalue.t
      ; init_value: Expression.t
      ; cmp_value: Expression.t
      ; direction: Direction.t }
    [@@deriving accessors, sexp, compare, equal]
  end

  (** {3 C-style loops}

      We provide a 'make' constructor for situations where some or all of the
      loop particulars are missing. *)

  (** Type of main loops. *)
  type t =
    {init: Assign.t option; cmp: Expression.t option; update: Assign.t option}
  [@@deriving accessors, make, sexp, compare, equal]

  val try_simplify : t -> Simple.t Or_error.t
  (** [try_simplify l] behaves like the getter for the accessor [simple], but
      gives a detailed error whenever the simplification fails. *)

  (** {4 Other accessors}

      See also the derived accessors [init], [cmp], and [update]. *)

  val init_opt : ('i, Assign.t, t, [< optional]) Accessor.t
  (** [init_opt] is [init], but as an optional accessor. *)

  val cmp_opt : ('i, Expression.t, t, [< optional]) Accessor.t
  (** [cmp_opt] is [cmp], but as an optional accessor. *)

  val update_opt : ('i, Assign.t, t, [< optional]) Accessor.t
  (** [update_opt] is [update], but as an optional accessor. *)

  val exprs : ('i, Expression.t, t, [< many]) Accessor.t
  (** [exprs] focuses on all expressions inside a for header. *)

  val simple : ('i, Simple.t, t, [< variant]) Accessor.t
  (** [simple] maps a C-style for header to, and from, a simplified header.
      Note that mapping to a simplified header is partial; to receive error
      feedback about why a particular C-style header isn't simplifiable, use
      [try_simplify]. *)
end

(** {2 Kinds of flow} *)

module While : sig
  (** Enumeration of types of while loop. *)
  type t = Do_while  (** A do-while loop. *) | While  (** A while loop. *)

  include Utils.Enum_types.Extension_table with type t := t
end

module Lock : sig
  (** Enumeration of types of lock block. *)
  type t =
    | Atomic  (** Atomic (strongly isolated transaction) blocks. *)
    | Synchronized
        (** Synchronised (weakly isolated pseudo-transaction) blocks. *)
  [@@deriving sexp, compare, equal]
end

(** {2 Headers proper} *)

module Header : sig
  (** Enumeration of flow block headers. *)
  type t =
    | For of For.t  (** A for-loop head. *)
    | Lock of Lock.t  (** A lock block. *)
    | While of While.t * Expression.t
        (** A while or do-while loop, alongside its condition. *)
    | Explicit  (** An explicit block with no conditions or looping. *)
    | Implicit
        (** An implicit block.

            Implicit blocks don't actually appear in reified code; they only
            serve to attach metadata to specific subranges of code. *)
  [@@deriving sexp, compare, equal]

  (** {3 Accessors and traversals} *)

  val exprs : ('i, Expression.t, t, [< many]) Accessor.t
  (** [exprs] focuses on all expressions inside a flow block header. *)

  (** Traversal over the expressions inside a header. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Expression.t

  (** Traversal over the lvalues inside a header. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t
end

(** {1 Flow blocks proper} *)

(** Type of while loops. *)
type ('meta, 'stm) t = {header: Header.t; body: ('meta, 'stm) Block.t}
[@@deriving sexp, make, accessors, compare, equal]

(** {2 Convenience constructors} *)

val while_loop :
     cond:Expression.t
  -> body:('meta, 'stm) Block.t
  -> kind:While.t
  -> ('meta, 'stm) t
(** [while ~cond ~body ~kind] makes a while loop with the given body,
    conditional, and kind. *)

val for_loop : ('meta, 'stm) Block.t -> control:For.t -> ('meta, 'stm) t
(** [for_loop body ~control] makes a for loop with control block [control]
    and body [body]. *)

val for_loop_simple :
  ('meta, 'stm) Block.t -> control:For.Simple.t -> ('meta, 'stm) t
(** [for_loop_simple body ~control] makes a simplified for loop with control
    block [control] and body [body]. *)

val lock_block : ('meta, 'stm) Block.t -> kind:Lock.t -> ('meta, 'stm) t
(** [lock_block body ~kind] makes a lock block with the given body and kind. *)

val explicit : ('meta, 'stm) Block.t -> ('meta, 'stm) t
(** [explicit b] makes an explicit block using [b]. *)

val implicit : ('meta, 'stm) Block.t -> ('meta, 'stm) t
(** [implicit b] makes an implicit block using [b]. *)

(** {2 Traversal helpers} *)

module Base_map (A : Applicative.S) : sig
  val bmap :
       ('m1, 's1) t
    -> header:(Header.t -> Header.t A.t)
    -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t A.t)
    -> ('m2, 's2) t A.t
  (** [bmap i ~header ~body] is a basic applicative map over flow blocks. *)
end

val map :
     ('m1, 's1) t
  -> header:(Header.t -> Header.t)
  -> body:(('m1, 's1) Block.t -> ('m2, 's2) Block.t)
  -> ('m2, 's2) t
(** [map i ~header ~body] is a basic map over flow blocks. *)
