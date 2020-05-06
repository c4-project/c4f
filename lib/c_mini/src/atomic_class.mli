(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Classifying atomic expressions and statements.

    For a general idea of what these 'classes' are, see {!Statement_class}. *)

(** Enumeration of atomic actions classes. *)
type t =
  | Cmpxchg  (** Atomic compare-exchanges. *)
  | Fence  (** Atomic fences. *)
  | Fetch  (** Atomic fetches. *)
  | Load  (** Atomic loads. *)
  | Store  (** Atomic stores. *)
  | Xchg  (** Atomic exchanges. *)

include Act_utils.Enum_types.Extension_table with type t := t

val matches : t -> template:t -> bool
(** [matches clazz ~template] checks whether [clazz] matches [template].
    Holes in [template] match any corresponding class in [clazz], but not
    vice versa. *)

(** {1 Classifiers} *)

val classify_expr : _ Atomic_expression.t -> t option
(** [classify_expr e] tries to classify the atomic expression [e]. *)

val classify_stm : Atomic_statement.t -> t option
(** [classify_stm s] tries to classify the atomic statement [s]. *)
