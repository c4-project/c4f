(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini C: translation unit *)

open Base

type t [@@deriving sexp]

(** {2 Constructors} *)

val make :
     globals:(Act_common.C_id.t, Mini_initialiser.t) List.Assoc.t
  -> functions:(Act_common.C_id.t, Mini_function.t) List.Assoc.t
  -> t
(** [make ~globals ~functions] makes a program with global variable
    declarations [globals] and function definitions [functions]. *)

(** {2 Accessors} *)

val globals : t -> (Act_common.C_id.t, Mini_initialiser.t) List.Assoc.t
(** [globals program] gets an associative list of each global initialiser in
    [program]. *)

val functions : t -> (Act_common.C_id.t, Mini_function.t) List.Assoc.t
(** [functions program] gets an associative list of each function in
    [program]. *)

val cvars : t -> Set.M(Act_common.C_id).t
(** [cvars program] extracts a set of C variable names from [program]. *)

(** {2 Mutators} *)

val with_functions :
  t -> (Act_common.C_id.t, Mini_function.t) List.Assoc.t -> t
(** [with_functions prog new_functions] creates a new program by
    substituting [new_functions] for [prog]'s functions. *)

(** {2 Traversals} *)

(** [On_decls] allows traversal over all of the declarations inside a
    program. *)
module On_decls :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Act_common.C_id.t * Mini_initialiser.t
