(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, while and do-while loops. *)

open Import

(** {1 Insertion} *)

module Insert : sig
  (** Module type of insertion actions.

      The expression generated forms the condition for the loop. *)
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Payload_impl.Cond_pathed.t

  (** This action inserts a while loop with a falsy expression in a random
      statement position; its body begins empty but is marked as dead-code. *)
  module False : S
end

(** {1 Surround} *)

module Surround : sig
  (** {2 While-loop surrounds} *)

  (** Module type of surround actions. *)
  module type S =
    Fuzz.Action_types.S with type Payload.t = Fuzz.Payload_impl.Cond_pathed.t

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
  module Dead : S
end
