(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
module type S_common = sig
  type 'meta t [@@deriving sexp]

  (** We can traverse over the metadata. *)
  module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t

  val erase_meta : 'meta t -> unit t
  (** [erase_meta x] deletes all of [x]'s metadata. *)

  (** By fixing the metadata type, we can perform various forms of standard
      traversal. *)
  module With_meta (Meta : T) : S_with_meta with type t := Meta.t t
end

(** {1 Parametrised statement signatures}

    These two signatures exist because their implementing modules are
    mutually recursive.

    The implementations of these generally fix the type parameters at the top
    of the signature. *)

(** {2 Statements}

    Parametrised signature of statement implementations. *)
module type S_statement = sig
  type 'meta t

  (** Generally fixed to {!Statement.If.t}. *)
  type 'meta if_stm

  (** Generally fixed to {!Statement.While.t}. *)
  type 'meta while_loop

  (** {3 Traversing} *)

  module Base_map (M : Monad.S) : sig
    val bmap :
         'm1 t
      -> prim:('m1 * Prim_statement.t -> ('m2 * Prim_statement.t) M.t)
      -> if_stm:('m1 if_stm -> 'm2 if_stm M.t)
      -> while_loop:('m1 while_loop -> 'm2 while_loop M.t)
      -> 'm2 t M.t
  end

  include S_common with type 'meta t := 'meta t
end
