(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: translation unit *)

open Base

type 'meta t [@@deriving sexp]

(** {2 Constructors} *)

val make :
     globals:(C4f_common.C_id.t, Initialiser.t) List.Assoc.t
  -> functions:(C4f_common.C_id.t, 'meta Function.t) List.Assoc.t
  -> 'meta t
(** [make ~globals ~functions] makes a program with global variable
    declarations [globals] and function definitions [functions]. *)

(** {2 Accessors} *)

val globals : _ t -> (C4f_common.C_id.t, Initialiser.t) List.Assoc.t
(** [globals program] gets an associative list of each global initialiser in
    [program]. *)

val functions : 'meta t -> (C4f_common.C_id.t, 'meta Function.t) List.Assoc.t
(** [functions program] gets an associative list of each function in
    [program]. *)

(** {2 Mutators} *)

val with_functions :
  _ t -> (C4f_common.C_id.t, 'meta Function.t) List.Assoc.t -> 'meta t
(** [with_functions prog new_functions] creates a new program by substituting
    [new_functions] for [prog]'s functions. *)

(** {2 Traversals} *)

module With_meta (Meta : Equal.S) : sig
  type nonrec t = Meta.t t

  (** [On_decls] allows traversal over all of the declarations inside a
      program. *)
  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.t C4f_common.C_named.t
end
