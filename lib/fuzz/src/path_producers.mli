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

(** A path producer that takes in {!Subject.Block.t} and produces
    {!Path.Stms.t}. *)
module Block :
  Path_types.S_producer
    with type t = Path.Stms.t
     and type target = Subject.Block.t

(** A path producer that takes in {!Subject.Statement.If.t} and produces
    {!Path.If.t}. *)
module If :
  Path_types.S_producer
    with type t = Path.If.t
     and type target = Subject.Statement.If.t

(** A path producer that takes in {!Subject.Statement.Loop.t} and produces
    {!Path.Loop.t}. *)
module Loop :
  Path_types.S_producer
    with type t = Path.Loop.t
     and type target = Subject.Statement.Loop.t

(** A path producer that takes in {!Subject.Statement.t} and produces
    {!Path.Stm.t}. *)
module Statement :
  Path_types.S_producer
    with type t = Path.Stm.t
     and type target = Subject.Statement.t

(** A path producer that takes in {!Subject.Thread.t} and produces
    {!Path.Thread.t}. *)
module Thread :
  Path_types.S_producer
    with type t = Path.Thread.t
     and type target = Subject.Thread.t

(** A path producer that takes in {!Subject.Test.t} and produces
    {!Path.Program.t}. *)
module Test :
  Path_types.S_producer
    with type t = Path.Program.t
     and type target = Subject.Test.t
