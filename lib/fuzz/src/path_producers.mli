(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Producers of fuzzer subject paths.

    These take the form of modules that expose partial functions from
    specific parts of a fuzzer subject to Quickcheck generators over paths.

    Each contains two types: [t], which is the type of the path itself, and
    [target], which is the type of the subject segment that must be supplied
    as context. *)

(** {1 Path producer modules} *)

(** A path producer that takes in {!Subject.Statement.t} and produces
    {!Path.stm_list}. *)
module Statement_list :
  Path_types.S_producer
    with type t = Path.stm_list
     and type target = Subject.Statement.t list

(** A path producer that takes in {!Subject.Statement.If.t} and produces
    {!Path.ifs}. *)
module If_statement :
  Path_types.S_producer
    with type t = Path.ifs
     and type target = Subject.Statement.If.t

(** A path producer that takes in {!Subject.Statement.t} and produces
    {!Path.stm}. *)
module Statement :
  Path_types.S_producer
    with type t = Path.stm
     and type target = Subject.Statement.t

(** A path producer that takes in {!Subject.Thread.t} and produces
    {!Path.func}. *)
module Thread :
  Path_types.S_producer
    with type t = Path.func
     and type target = Subject.Thread.t

(** A path producer that takes in {!Subject.Test.t} and produces
    {!Path.program}. *)
module Test :
  Path_types.S_producer
    with type t = Path.program
     and type target = Subject.Test.t
