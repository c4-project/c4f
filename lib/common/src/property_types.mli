(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

(** Signature of Blang-compatible property enumerations. *)
module type S = sig
  type t [@@deriving sexp]
  (** Type of individual property elements. *)

  (** {2 Documentation} *)

  val names : string list Lazy.t
  (** Lazy-list of names of all properties. *)

  val tree_docs : Property.Tree_doc.t
  (** The documentation tree inspected by {!pp_tree}. *)

  val pp_tree : unit Fmt.t
  (** [pp_tree f ()] should print a tree representation of the properties
      available in this module to [f]. *)
end

(** Signature of Blang-compatible property enumerations that evaluate to
    Booleans. *)
module type S_bool = sig
  include S

  (** {2 Evaluation} *)

  type target

  val eval_b : t Blang.t -> target -> bool
  (** [eval_b pred target] evaluates whether the Blang predicate [pred] is
      true over [target]. *)
end
