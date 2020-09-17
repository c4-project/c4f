(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, loops. *)

open Import

(** {1 While and do-while *)

module While : sig

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
    module False : S
  end

  (** {1 Surround} *)

  module Surround : sig
    (** {2 While-loop surrounds} *)

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
    module Dead : S
  end
end

(** {1 For-loop surrounds} *)

module For : sig

  (** {2 Payloads} *)

  module Counter : sig
    (** Type of loop counters in payloads. *)
    type t = {var: Common.Litmus_id.t; ty: Fir.Type.t} [@@deriving sexp]
  end


  module Simple_payload : sig
    (** Type of payloads for simple for-loop actions. *)
    type t =
      {lc: Counter.t; up_to: Fir.Constant.t; where: Fuzz.Path.Flagged.t}
    [@@deriving sexp]
  end

  module Kv_payload : sig
    (** Type of payloads for known-value for-loop actions. *)
    type t =
      { lc: Counter.t
      ; kv_val: Act_fir.Constant.t
      ; kv_expr: Act_fir.Expression.t
      ; where: Act_fuzz.Path.Flagged.t }
    [@@deriving sexp]
  end

  (** {2 Surround actions} *)
  module Surround : sig
    (** Action that surrounds things with for-loops with a small, constant
        number of iterations. *)
    module Simple :
      Act_fuzz.Action_types.S with type Payload.t = Simple_payload.t

    (** Action that surrounds things with for-loops guaranteed statically to
        evaluate only once, by use of a known value *)
    module Kv_once :
      Act_fuzz.Action_types.S with type Payload.t = Kv_payload.t
  end
end
