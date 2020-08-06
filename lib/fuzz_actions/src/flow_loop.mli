(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, loops. *)

(** {1 Insertion} *)

module Insert : sig
  (** Module type of insertion actions.

      The expression generated forms the condition for the loop. *)
  module type S =
    Act_fuzz.Action_types.S
      with type Payload.t =
            Act_fir.Expression.t Act_fuzz.Payload_impl.Insertion.t

  (** This action inserts a while loop with a falsy expression in a random
      statement position; its body begins empty but is marked as dead-code. *)
  module While_false : S
end

(** {1 Surround} *)

module Surround : sig
  (** Module type of surround actions. *)
  module type S =
    Act_fuzz.Action_types.S
      with type Payload.t = Act_fuzz.Payload_impl.Cond_surround.t

  (** This action removes a sublist of statements from the program, replacing
      them with a `do... while` statement containing some transformation of
      the removed statements and a falsy expression. *)
  module Do_false : S

  (** This action removes a sublist of dead-code statements from the program,
      replacing them with a `do... while` statement containing some
      transformation of the removed statements and an arbitrary expression. *)
  module Do_dead : S

  (** This action removes a sublist of dead-code statements from the program,
      replacing them with a `while` statement containing some transformation
      of the removed statements and an arbitrary expression. *)
  module While_dead : S
end
