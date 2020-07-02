(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Producers of fuzzer subject paths.

    These take the form of partial functions from fuzzer subjects to
    Quickcheck generators over paths. *)

(** {1 Path producers} *)

val try_gen_insert_stm :
  ?filter:Path_filter.t -> Subject.Test.t -> Path.Test.t Opt_gen.t
(** [try_gen_insert_stm dest] tries to create a Quickcheck-style generator
    for statement insertion paths targeting [dest]. These paths can also be
    used for inserting statement lists.

    It can return an error if [dest] has no position at which statements can
    be inserted. *)

val try_gen_transform_stm_list :
  ?filter:Path_filter.t -> Subject.Test.t -> Path.Test.t Opt_gen.t
(** [try_gen_transform_stm dest] tries to create a Quickcheck-style generator
    for statement list transformation paths targeting [dest].

    It can return an error if [dest] has no position at which statement lists
    can be transformed. *)

val try_gen_transform_stm :
  ?filter:Path_filter.t -> Subject.Test.t -> Path.Test.t Opt_gen.t
(** [try_gen_transform_stm ?predicate dest] tries to create a
    Quickcheck-style generator for statement transformation paths targeting
    [dest], and, optionally, satisfying [filter]. It returns an error if the
    container is empty or no such statements were found. *)
