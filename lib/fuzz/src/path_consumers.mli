(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Consumers of fuzzer subject paths.

    These take the form of modules that expose functions that accept paths
    and subject components, and perform actions on the part of that subject
    referenced by the path.

    Each contains two types: [t], which is the type of the path itself, and
    [target], which is the type of the target subject component. *)

(** {1 Path consumer modules} *)

(** A path consumer that acts upon a {!Subject.Block.t} according to a
    {!Path.Stms.t}. *)
module Block :
  Path_types.S_consumer
    with type t = Path.Stms.t
     and type target = Subject.Block.t

(** A path consumer that acts upon a {!Subject.Statement.If.t} according to a
    {!Path.If.t}. *)
module If :
  Path_types.S_consumer
    with type t = Path.If.t
     and type target = Subject.Statement.If.t

(** A path consumer that acts upon a {!Subject.Statement.Flow.t} according to
    a {!Path.Flow.t}. *)
module Flow :
  Path_types.S_consumer
    with type t = Path.Flow.t
     and type target = Subject.Statement.Flow.t

(** A path consumer that acts upon a {!Subject.Statement.t} according to a
    {!Path.Stm.t}. *)
module Statement :
  Path_types.S_consumer
    with type t = Path.Stm.t
     and type target = Subject.Statement.t

(** A path consumer that acts upon a {!Subject.Thread.t} according to a
    {!Path.Thread.t}. *)
module Thread :
  Path_types.S_consumer
    with type t = Path.Thread.t
     and type target = Subject.Thread.t

(** A path consumer that acts upon a {!Subject.Test.t} according to a
    {!Path.Program.t}. *)
module Test :
  Path_types.S_consumer
    with type t = Path.Program.t
     and type target = Subject.Test.t
