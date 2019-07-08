(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini C: functions (less their names). *)

open Base

type t [@@deriving sexp, equal]

(** {2 Constructors} *)

val make :
     parameters:(Act_common.C_id.t, Type.t) List.Assoc.t
  -> body_decls:(Act_common.C_id.t, Initialiser.t) List.Assoc.t
  -> ?body_stms:Statement.t list
  -> unit
  -> t
(** [make ~parameters ~body_decls ?body_stms] creates a function with the
    given contents. *)

(** {2 Accessors} *)

val parameters : t -> (Act_common.C_id.t, Type.t) List.Assoc.t
(** [parameters func] gets [func]'s parameter list. *)

val body_decls : t -> (Act_common.C_id.t, Initialiser.t) List.Assoc.t
(** [body_decls func] gets [func]'s in-body variable declarations. *)

val body_stms : t -> Statement.t list
(** [body_decls func] gets [func]'s statements. *)

val cvars : t -> Set.M(Act_common.C_id).t
(** [cvars func] extracts a set of C variable names from [func]. *)

(** {3 Mutators} *)

val with_body_stms : t -> Statement.t list -> t
(** [with_body_stms func new_stms] produces a new function by substituting
    [new_stms] for [func]'s body statements. *)

val map :
     t
  -> parameters:(   (Act_common.C_id.t, Type.t) List.Assoc.t
                 -> (Act_common.C_id.t, Type.t) List.Assoc.t)
  -> body_decls:(   (Act_common.C_id.t, Initialiser.t) List.Assoc.t
                 -> (Act_common.C_id.t, Initialiser.t) List.Assoc.t)
  -> body_stms:(Statement.t list -> Statement.t list)
  -> t
(** [map func ~parameters ~body_decls ~body_stms] runs the given functions
    over the respective parts of a function. *)

(** {3 Traversals} *)

module On_monad (M : Monad.S) : sig
  val map_m :
       t
    -> parameters:(   (Act_common.C_id.t, Type.t) List.Assoc.t
                   -> (Act_common.C_id.t, Type.t) List.Assoc.t M.t)
    -> body_decls:(   (Act_common.C_id.t, Initialiser.t) List.Assoc.t
                   -> (Act_common.C_id.t, Initialiser.t) List.Assoc.t
                      M.t)
    -> body_stms:(Statement.t list -> Statement.t list M.t)
    -> t M.t
end

(** [On_decls] allows traversal over all of the declarations inside a
    function. *)
module On_decls :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Initialiser.t Named.t
