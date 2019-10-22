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
    t -> Metadata.t Act_c_mini.Statement.t -> target -> target Or_error.t
  (** [insert_stm path stm dest] tries to insert [stm] into the part of
      [dest] pointed to by [path]. *)

  val transform_stm :
       t
    -> f:
         (   Metadata.t Act_c_mini.Statement.t
          -> Metadata.t Act_c_mini.Statement.t Or_error.t)
    -> target
    -> target Or_error.t
  (** [transform_stm path ~f dest] tries to modify the statement at [stm]
      using [f]. *)
end

(** Signature of paths over statements and statement-like entities. *)
module type S_statement = sig
  type target

  include S_path with type t := Path_shapes.stm and type target := target

  val try_gen_insert_stm :
    target -> Path_shapes.stm Base_quickcheck.Generator.t option
  (** [try_gen_insert_stm dest] tries to create a Quickcheck-style generator
      for statement insertion paths targeting [dest].

      It can return [None] if [dest] has no position at which statements can
      be inserted. *)
end

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
      with type t := Path_shapes.stm_list
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