(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
    C4f_fuzz.Action_types.S
      with type Payload.t = C4f_fuzz.Payload_impl.Cond_pathed.t

  (** Generates if statements where the expression is arbitrary and both
      blocks contain the original statement span. *)
  module Duplicate : S

  (** Generates if statements where the expression is a tautology, the first
      block contains the original statement span, and the second block is an
      empty dead-code block. *)
  module Tautology : S
end

module Transform : sig
  (** Basic transform generators have this type. *)
  module type S =
    C4f_fuzz.Action_types.S with type Payload.t = C4f_fuzz.Path.With_meta.t

  (** Inverts existing if statements. *)
  module Invert : S
end
