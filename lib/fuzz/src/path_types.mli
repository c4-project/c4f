(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for mini-C path-based traversal. *)

open Base

(** General signature of paths. *)
module type S_path = sig
  type t
  (** Type of paths. *)

  type target

  val insert_stm :
       t
    -> to_insert:Metadata.t Act_c_mini.Statement.t
    -> target:target
    -> target Or_error.t
  (** [insert_stm path ~to_insert ~target] tries to insert [to_insert] into
      [path] relative to [target]. *)

  val transform_stm :
       t
    -> f:
         (   Metadata.t Act_c_mini.Statement.t
          -> Metadata.t Act_c_mini.Statement.t Or_error.t)
    -> target:target
    -> target Or_error.t
  (** [transform_stm path ~f ~target] tries to modify the statement at [path]
      relative to [target] using [f]. *)

  val transform_stm_list :
       t
    -> f:
         (   Metadata.t Act_c_mini.Statement.t list
          -> Metadata.t Act_c_mini.Statement.t list Or_error.t)
    -> target:target
    -> target Or_error.t
  (** [transform_stm_list path ~f ~target] tries to modify the list of all
      statement at [path] relative to [target] using [f]. Unlike
      {!transform_stm}, [transform_stm_list] can add and remove statements
      from the enclosing block. *)

  val try_gen_transform_stm_list :
    target -> t Base_quickcheck.Generator.t option
  (** [try_gen_transform_stm dest] tries to create a Quickcheck-style
      generator for statement list transformation paths targeting [dest].

      It can return [None] if [dest] has no position at which statement lists
      can be transformed. *)

  val try_gen_transform_stm :
       ?predicate:(Subject.Statement.t -> bool)
    -> target
    -> t Base_quickcheck.Generator.t option
  (** [try_gen_transform_stm ?predicate dest] tries to create a
      Quickcheck-style generator for statement transformation paths targeting
      [dest], and, optionally, reaching statements satisfying the predicate
      [predicate]. It returns [None] if the container is empty or no such
      statements were found. *)
end

(** Signature of paths over statements and statement-like entities. *)
module type S_statement = sig
  type target

  include S_path with type t = Path_shapes.stm and type target := target

  val try_gen_insert_stm : target -> t Base_quickcheck.Generator.t option
  (** [try_gen_insert_stm dest] tries to create a Quickcheck-style generator
      for statement insertion paths targeting [dest].

      It can return [None] if [dest] has no position at which statements can
      be inserted. *)
end

(** Signature of paths over statement containers (such as lists).

    The key difference between this and path modules over statements
    themselves is that generating a statement insertion path is guaranteed to
    succeed. *)
module type S_stm_container = sig
  include S_path

  val gen_insert_stm : target -> t Base_quickcheck.Generator.t
  (** [gen_insert_stm dest] creates a Quickcheck-style generator for
      statement insertion paths targeting [dest]. *)
end

(** Signature of paths over conditionals *)
module type S_if_statement = sig
  type target

  include S_path with type t := Path_shapes.ifs and type target := target

  include
    S_stm_container with type t := Path_shapes.ifs and type target := target
end

(** Signature of paths over statement lists *)
module type S_statement_list = sig
  type target

  include
    S_stm_container
      with type t = Path_shapes.stm_list
       and type target := target list
end

(** Signature of paths over functions *)
module type S_function = sig
  include S_stm_container with type t := Path_shapes.func
end

(** Signature of paths over programs *)
module type S_program = sig
  include S_stm_container with type t := Path_shapes.program
end
