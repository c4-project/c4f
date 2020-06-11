(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module signatures for expressions.

    These signatures exist because expressions are defined in mutually
    recursive modules. *)

(** Base type of expression modules that support traversals. *)
module type S_traversable = sig
  (** Type of traversable expression components. *)
  type t

  (** Traversing over atomic-action addresses in expressions. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

  (** Traversing over memory orders in expressions. *)
  module On_mem_orders :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mem_order.t

  (** Traversing over constants in expressions. *)
  module On_constants :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t
end

(** Basic signature for atomic-expression submodules. *)
module type S_atomic = sig
  (** Type of atomic expression, parametrised by expression type. *)
  type 'e t

  val quickcheck_observer :
    'e Base_quickcheck.Observer.t -> 'e t Base_quickcheck.Observer.t
  (** [quickcheck_observer expr] is a generic observer, exposed here to let
      expressions (with observer [expr]) observe subcomponents. *)

  (** [On_expressions] permits traversing over the expressions inside an
      atomic expression. *)
  module On_expressions : Travesty.Traversable_types.S1 with type 'e t = 'e t

  (** We can type-check atomic types whose expressions have already been
      type-checked. *)
  include Types.S_type_checkable with type t := Type.t t
end
