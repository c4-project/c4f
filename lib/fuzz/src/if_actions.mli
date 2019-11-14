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
  (** {2 Payload}

      The payload for any if-surround action contains two elements:

      - the expression to insert into the condition of the if statement;
      - the path to the statements to remove, pass through the if statement
        block generators, and replace with the statement. *)

  module Payload : sig
    type t
    (** Opaque type of payloads. *)

    val make : cond:Act_c_mini.Expression.t -> path:Path_shapes.program -> t
    (** [make ~cond ~path] makes a payload given a specific condition
        expression [cond] and statement-list selecting path [path]. *)
  end

  module type S = Action_types.S with type Payload.t = Payload.t
  (** Each if statement generator has this type. *)

  module Duplicate : S
  (** Generates if statements where the expression is arbitrary and both
      blocks contain the original statement span. *)

  module Tautology : S
  (** Generates if statements where the expression is a tautology, the first
      block contains the original statement span, and the second block is an
      empty dead-code block. *)
end

(** {1 Inverting existing if statements} *)

module Invert : Action_types.S with type Payload.t = Path_shapes.program
