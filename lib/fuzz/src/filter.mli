(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The fuzzer, packaged up as filters.

    The fuzzer takes various pieces of configuration (see {{!Aux} Aux}) as
    auxiliary input, and emits a trace of actions performed (see {{!Trace}
    Trace}) as auxiliary output. *)

(** {1 Fuzzer config as an auxiliary input} *)

module Aux : sig
  (** Opaque type of auxiliary input to the fuzzer filter. The [rest]
      parameter depends on the specific filter. *)
  type 'rest t

  val make : ?o:Act_common.Output.t -> config:Config.t -> 'rest -> 'rest t
  (** [make ?o ~config rest] makes an auxiliary input for the fuzzer filters
      given optional outputter [o], fuzzer config [config], and
      fuzzer-dependent 'rest of input' parameter [rest]. *)
end

(** {1 Running the fuzzer with random actions}

    Here, the 'rest' parameter to the auxiliary input is an optional seed to
    use to start the random number generator. *)
module Random :
  Plumbing.Filter_types.S
    with type aux_i = int option Aux.t
     and type aux_o = Trace.t

(** {1 Replaying a trace with the fuzzer}

    Here, the 'rest' parameter to the auxiliary input is the trace to replay. *)
module Replay :
  Plumbing.Filter_types.S
    with type aux_i = Trace.t Aux.t
     and type aux_o = unit
