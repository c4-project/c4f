(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** FIR: function calls

    Note that many things that appear in a C AST as function calls actually
    map to primitives: for example, atomic stores become {!Atomic_store.t}. *)

(** Opaque type of function calls. *)
type t [@@deriving sexp, compare, equal]

(** {1 Constructors} *)

val make :
  ?arguments:Expression.t list -> function_id:C4f_common.C_id.t -> unit -> t
(** [make ?arguments ~metadata ~function_id ()] constructs a function call
    with the metadata [metadata], function ID [function_id], and arguments
    [arguments]. *)

(** {1 Accessors} *)

val function_id : t -> C4f_common.C_id.t
(** [function_id call] gets the identifier of the function called by [call]. *)

val arguments : t -> Expression.t list
(** [arguments call] gets the argument expressions of [call]. *)

(** {1 Traversals} *)

(** Traverses over the addresses of a function call. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traverses over the expressions of a function call. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t
