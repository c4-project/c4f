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

open Base

(** {1 Path producers proper} *)

val produce_seq :
     ?filter:Path_filter.t
  -> Subject.Test.t
  -> kind:Path_kind.t
  -> Path.With_meta.t Sequence.t
(** [produce_seq ?filter test ~kind] produces a lazy sequence of all paths of
    kind [kind] over test [test] that match filter [filter]. *)

val is_constructible :
  ?filter:Path_filter.t -> Subject.Test.t -> kind:Path_kind.t -> bool
(** [is_constructible ?filter test ~kind] checks whether there exists at
    least one valid path of kind [kind] over test [test] that matches filter
    [filter]. *)

val try_gen :
     ?filter:Path_filter.t
  -> Subject.Test.t
  -> kind:Path_kind.t
  -> Path.With_meta.t Opt_gen.t
(** [try_gen ?filter test ~kind] tries to choose a random path of kind [kind]
    over test [test] that matches filter [filter]. It returns the path
    alongside the flags that were enabled on it, and fails if such a path is
    not constructible. *)
