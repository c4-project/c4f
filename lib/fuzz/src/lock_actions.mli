(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, locks. *)

(** {1 Surrounding with transactions} *)

(** This action removes a sublist of statements from the program, replacing
    them with an `synchronized` statement containing those statements.

    See also {!If_actions.Surround}. *)
module Sync_surround : Action_types.S with type Payload.t = Path.Program.t

(** This action removes a sublist of statements from the program, replacing
    them with an `atomic` statement containing those statements.

    It only fires if the statements are transaction-safe.

    See also {!If_actions.Surround}. *)
module Atomic_surround : Action_types.S with type Payload.t = Path.Program.t
