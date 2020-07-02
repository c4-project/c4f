(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, if statements. *)

(** {1 Actions that surround statements in if statements}

    The general idea of such a generator is that it takes:

    - a condition expression generator in the form of an action payload;
    - a mapping from an existing statement list to the 'true' block of the
      statement generator;
    - a similar mapping to the 'false' block. *)
module Surround : sig
  (** Each if statement generator has this type. *)
  module type S =
    Action_types.S with type Payload.t = Payload.Cond_surround.t

  (** Generates if statements where the expression is arbitrary and both
      blocks contain the original statement span. *)
  module Duplicate : S

  (** Generates if statements where the expression is a tautology, the first
      block contains the original statement span, and the second block is an
      empty dead-code block. *)
  module Tautology : S
end

(** {1 Inverting existing if statements} *)

module Invert : Action_types.S with type Payload.t = Path.Test.t
