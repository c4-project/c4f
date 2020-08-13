(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, locks. *)

(** {1 Surrounding with transactions} *)
module Surround : sig
  (** Type of surround modules. *)
  module type S =
    Act_fuzz.Action_types.S with type Payload.t = Act_fuzz.Path.Flagged.t

  (** This action removes a sublist of statements from the program, replacing
      them with an `synchronized` statement containing those statements. *)
  module Sync : S

  (** This action removes a sublist of statements from the program, replacing
      them with an `atomic` statement containing those statements.

      It only fires if the statements are transaction-safe. *)
  module Atomic : S
end
