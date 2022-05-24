(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: recursive module signatures for statement traversal

    You'll usually want {!If}, {!While}, {!Statement}, etc. *)

open Base

(** Module type defining various traversals over statements that depend on
    knowing the metadata of a statement. *)
module type S_with_meta = sig
  (* Type of resolved statement metadata. *)
  type t

  (** Traversing over atomic-action addresses. *)
  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

  (** Traversing over expressions. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Expression.t

  (** Traversing over lvalues. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

  (** Traversing over primitive statements. *)
  module On_primitives :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Prim_statement.t
end

(** Common functionality of statement components. *)
module type S_traversable = sig
  type 'meta t

  (** We can traverse over the metadata. *)
  module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t

  val erase_meta : 'meta t -> unit t
  (** [erase_meta x] deletes all of [x]'s metadata. *)

  (** By fixing the metadata type, we can perform various forms of standard
      traversal. *)
  module With_meta (Meta : T) : S_with_meta with type t := Meta.t t
end
