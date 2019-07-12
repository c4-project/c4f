(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: expressions *)

type t [@@deriving sexp, compare, equal]
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

val lvalue : Lvalue.t -> t
(** [lvalue lv] lifts a lvalue [lv] to an expression. *)

(** {2 Accessors} *)

val reduce :
     t
  -> bool_lit:(bool -> 'a)
  -> constant:(Act_c_lang.Ast_basic.Constant.t -> 'a)
  -> lvalue:(Lvalue.t -> 'a)
  -> atomic_load:(Atomic_load.t -> 'a)
  -> eq:('a -> 'a -> 'a)
  -> 'a
(** [reduce expr ~bool_lit ~constant ~lvalue ~atomic_load ~eq] recursively
    reduces [expr] to a single value, using the given functions at each
    corresponding stage of the expression tree. *)

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in expressions. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over identifiers in expressions. *)
module On_identifiers :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Act_c_lang.Ast_basic.Identifier.t

(** Traversing over lvalues in expressions. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {2 Generation and quickchecking}

    See {{!Expression_gen} Expression_gen}. *)

val quickcheck_observer : t Base_quickcheck.Observer.t
(** Blanket observer for all expression generators. *)

(** {2 Type checking} *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for expressions. *)
