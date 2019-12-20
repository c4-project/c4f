(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Collections of default IDs, used for resolving ambiguous references. *)

open Base

type t [@@deriving sexp]
(** Opaque type of default ID collections. *)

(** {2 Constructors} *)

val make :
     ?arches:Act_common.Id.t list
  -> ?compilers:Act_common.Id.t list
  -> ?machines:Act_common.Id.t list
  -> ?backends:Act_common.Id.t list
  -> unit
  -> t
(** [make ?arches ?compilers ?machines ?backends ()] constructs a default ID
    environment with the given bin contents. *)

val of_ast : Ast.Default.t list -> t Or_error.t
(** [of_ast stanza] constructs a default ID environment from the config file
    AST stanza [stanza]. *)

(** {2 Projections}

    All ID-list projections are in descending precedence order: the first ID
    is the one that should be tried first. *)

val arches : t -> Act_common.Id.t list
(** [arches ds] gets the list of default architecture IDs from [ds]. *)

val compilers : t -> Act_common.Id.t list
(** [compilers ds] gets the list of default compiler IDs from [ds]. *)

val machines : t -> Act_common.Id.t list
(** [machines ds] gets the list of default machine IDs from [ds]. *)

val backends : t -> Act_common.Id.t list
(** [backends ds] gets the list of default backend IDs from [ds]. *)

val to_ast : t -> Ast.Default.t list
(** [to_ast ds] converts [ds] into its abstract syntax tree representation. *)
