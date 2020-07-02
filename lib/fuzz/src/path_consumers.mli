(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Consumers of fuzzer subject paths.

    These take the form of functions that accept paths and tests, and perform
    actions on the part of that subject referenced by the path. *)

open Base

val check_path :
     Path.Test.t
  -> filter:Path_filter.t
  -> target:Subject.Test.t
  -> Subject.Test.t Or_error.t
(** [check_path path ~filter ~target] does a post-generation check of [path]
    against the path filter [filter] and target [target].

    Don't confuse this with the in-generation checks done in
    {!Path_producers}, which serve to stop invalid paths from being
    generated. The purpose of *this* check is to protect fuzzer actions
    against generation errors, stale traces, and badly written test cases. *)

val insert_stm_list :
     Path.Test.t
  -> to_insert:Metadata.t Act_fir.Statement.t list
  -> target:Subject.Test.t
  -> Subject.Test.t Or_error.t
(** [insert_stm_list path ~to_insert ~target] tries to insert each statement
    in [to_insert] into [path] relative to [target], in order. *)

val insert_stm :
     Path.Test.t
  -> to_insert:Metadata.t Act_fir.Statement.t
  -> target:Subject.Test.t
  -> Subject.Test.t Or_error.t
(** [insert_stm path ~to_insert ~target] tries to insert [to_insert] into
    [path] relative to [target]. *)

val transform_stm :
     Path.Test.t
  -> f:
       (   Metadata.t Act_fir.Statement.t
        -> Metadata.t Act_fir.Statement.t Or_error.t)
  -> target:Subject.Test.t
  -> Subject.Test.t Or_error.t
(** [transform_stm path ~f ~target] tries to modify the statement at [path]
    relative to [target] using [f]. *)

val transform_stm_list :
     Path.Test.t
  -> f:
       (   Metadata.t Act_fir.Statement.t list
        -> Metadata.t Act_fir.Statement.t list Or_error.t)
  -> target:Subject.Test.t
  -> Subject.Test.t Or_error.t
(** [transform_stm_list path ~f ~target] tries to modify the list of all
    statement at [path] relative to [target] using [f]. Unlike
    {!transform_stm}, [transform_stm_list] can add and remove statements. *)
