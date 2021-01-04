(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: functions (less their names). *)

open Base

type 'meta t [@@deriving sexp, equal]

(** {2 Constructors} *)

val make :
     parameters:(C4f_common.C_id.t, Type.t) List.Assoc.t
  -> body_decls:(C4f_common.C_id.t, Initialiser.t) List.Assoc.t
  -> ?body_stms:'meta Statement.t list
  -> unit
  -> 'meta t
(** [make ~parameters ~body_decls ?body_stms] creates a function with the
    given contents. *)

(** {2 Accessors} *)

module Access : sig
  val parameters :
    ( _
    , (C4f_common.C_id.t, Type.t) List.Assoc.t
    , _ t
    , [< Accessor.field] )
    Accessor.Simple.t
  (** [parameters] accesses a function's parameter list. *)

  val body_decls :
    ( _
    , (C4f_common.C_id.t, Initialiser.t) List.Assoc.t
    , _ t
    , [< Accessor.field] )
    Accessor.Simple.t
  (** [body_decls] accesses a function's in-body variable declarations. *)

  val body_stms :
    ( 'i -> 'm1 Statement.t list -> 'm2 Statement.t list
    , 'i -> 'm1 t -> 'm2 t
    , [< Accessor.field] )
    Accessor.t
  (** [body_stms] accesses a function's statements. *)
end

(** {2 Getters} *)

val parameters : _ t -> (C4f_common.C_id.t, Type.t) List.Assoc.t
(** [parameters func] gets [func]'s parameter list. *)

val body_decls : _ t -> (C4f_common.C_id.t, Initialiser.t) List.Assoc.t
(** [body_decls func] gets [func]'s in-body variable declarations. *)

val body_stms : 'meta t -> 'meta Statement.t list
(** [body_stms func] gets [func]'s statements. *)

(** {3 Mutators} *)

val with_body_stms : 'm1 t -> 'm2 Statement.t list -> 'm2 t
(** [with_body_stms func new_stms] produces a new function by substituting
    [new_stms] for [func]'s body statements. *)

val map :
     'm1 t
  -> parameters:
       (   (C4f_common.C_id.t, Type.t) List.Assoc.t
        -> (C4f_common.C_id.t, Type.t) List.Assoc.t)
  -> body_decls:
       (   (C4f_common.C_id.t, Initialiser.t) List.Assoc.t
        -> (C4f_common.C_id.t, Initialiser.t) List.Assoc.t)
  -> body_stms:('m1 Statement.t list -> 'm2 Statement.t list)
  -> 'm2 t
(** [map func ~parameters ~body_decls ~body_stms] runs the given functions
    over the respective parts of a function. *)

(** {3 Traversals} *)

module On (M : Applicative.S) : sig
  val map_m :
       'm1 t
    -> parameters:
         (   (C4f_common.C_id.t, Type.t) List.Assoc.t
          -> (C4f_common.C_id.t, Type.t) List.Assoc.t M.t)
    -> body_decls:
         (   (C4f_common.C_id.t, Initialiser.t) List.Assoc.t
          -> (C4f_common.C_id.t, Initialiser.t) List.Assoc.t M.t)
    -> body_stms:('m1 Statement.t list -> 'm2 Statement.t list M.t)
    -> 'm2 t M.t
end

module With_meta (Meta : Equal.S) : sig
  type nonrec t = Meta.t t [@@deriving equal]

  (** [On_decls] allows traversal over all of the declarations inside a
      function. *)
  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.t C4f_common.C_named.t
end
