(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, locks. *)

open Import

(** {1 Surrounding with transactions} *)
module Surround : sig
  (** Type of surround modules.

      Ideally, we would use just {!Fuzz.Path.With_meta.t} as the payload
      here. However, {!Fuzz.Action.Make_surround} expects a payload of the
      form [t Fuzz.Payload_impl.Pathed.t]. *)
  module type S =
    Fuzz.Action_types.S with type Payload.t = unit Fuzz.Payload_impl.Pathed.t

  (** This action removes a sublist of statements from the program, replacing
      them with an `synchronized` statement containing those statements. *)
  module Sync : S

  (** This action removes a sublist of statements from the program, replacing
      them with an `atomic` statement containing those statements.

      It only fires if the statements are transaction-safe. *)
  module Atomic : S
end
