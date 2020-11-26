(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The fuzzer, packaged up as filters.

    Each filter takes in a Litmus test, and outputs another Litmus test; the
    auxiliary input and output concerns things like traces, seeds, and
    verbose output. *)

open Import

(** {1 Auxiliary inputs} *)

module Aux : sig
  (** {2 Auxiliary output for both filters}

      Note that, while the replay filter returns a trace, it is exactly the
      same trace as was put in. *)
  module Output : sig
    (** Type of auxiliary output. *)
    type t = {trace: Fuzz.Trace.t; state: Fuzz.State.t}
  end

  (** {2 Auxiliary input for the randomised runner} *)
  module Randomised : sig
    (** Opaque type of auxiliary input to the randomised runner filter. *)
    type t

    val make : ?seed:int -> ?o:Common.Output.t -> Config.t -> t
    (** [make ?seed ?o config] makes an auxiliary input for the randomised
        filter given optional seed [seed], optional outputter [o], and fuzzer
        config [config]. *)
  end

  (** {2 Auxiliary input for the replaying runner} *)
  module Replay : sig
    (** Opaque type of auxiliary input to the replay runner filter. *)
    type t

    val make : ?o:Common.Output.t -> Act_fuzz.Trace.t -> t
    (** [make ?o trace] makes an auxiliary input for the replay filter given
        optional outputter [o] and trace-to-replay [trace]. *)
  end
end

(** {1 Running the fuzzer with random actions}

    Here, the 'rest' parameter to the auxiliary input is an optional seed to
    use to start the random number generator. *)
module Randomised :
  Plumbing.Filter_types.S
    with type aux_i = Aux.Randomised.t
     and type aux_o = Aux.Output.t

(** {1 Replaying a trace with the fuzzer}

    Here, the 'rest' parameter to the auxiliary input is the trace to replay. *)
module Replay :
  Plumbing.Filter_types.S
    with type aux_i = Aux.Replay.t
     and type aux_o = Aux.Output.t
