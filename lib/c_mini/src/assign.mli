(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A non-atomic assignment. *)

type t [@@deriving sexp, equal]

(** {3 Constructors} *)

val make : lvalue:Lvalue.t -> rvalue:Expression.t -> t
(** [make ~lvalue ~rvalue] constructs an assignment of [rvalue] to [lvalue]. *)

(** {3 Accessors} *)

val lvalue : t -> Lvalue.t
(** [lvalue asn] gets [asn]'s destination lvalue. *)

val rvalue : t -> Expression.t
(** [rvalue asn] gets [asn]'s source expression (rvalue). *)

(** {3 Traversals} *)

(** Traversing over atomic-action addresses in assignments. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over lvalues in assignments. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t
