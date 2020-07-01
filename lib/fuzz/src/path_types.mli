(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types for fuzzer path producers and consumers. *)

open Base

(** {1 Basic signature}

    This signature is that common to both producers and consumers. *)
module type S_base = sig
  (** Type of paths. *)
  type t

  (** Type of path targets. *)
  type target
end

(** {1 Path consumers}

    General signature of path consumers. *)
module type S_consumer = sig
  include S_base

  val check_path :
    t -> filter:Path_filter.t -> target:target -> target Or_error.t
  (** [check_path path ~filter ~target] does a post-generation check of
      [path] against the path filter [filter] and target [target].

      Don't confuse this with the in-generation checks done in
      {!Path_producers}, which serve to stop invalid paths from being
      generated. The purpose of *this* check is to protect fuzzer actions
      against generation errors, stale traces, and badly written test cases. *)

  val insert_stm_list :
       t
    -> to_insert:Metadata.t Act_fir.Statement.t list
    -> target:target
    -> target Or_error.t
  (** [insert_stm_list path ~to_insert ~target] tries to insert each
      statement in [to_insert] into [path] relative to [target], in order. *)

  val insert_stm :
       t
    -> to_insert:Metadata.t Act_fir.Statement.t
    -> target:target
    -> target Or_error.t
  (** [insert_stm path ~to_insert ~target] tries to insert [to_insert] into
      [path] relative to [target]. *)

  val transform_stm :
       t
    -> f:
         (   Metadata.t Act_fir.Statement.t
          -> Metadata.t Act_fir.Statement.t Or_error.t)
    -> target:target
    -> target Or_error.t
  (** [transform_stm path ~f ~target] tries to modify the statement at [path]
      relative to [target] using [f]. *)

  val transform_stm_list :
       t
    -> f:
         (   Metadata.t Act_fir.Statement.t list
          -> Metadata.t Act_fir.Statement.t list Or_error.t)
    -> target:target
    -> target Or_error.t
  (** [transform_stm_list path ~f ~target] tries to modify the list of all
      statement at [path] relative to [target] using [f]. Unlike
      {!transform_stm}, [transform_stm_list] can add and remove statements
      from the enclosing block. *)
end
