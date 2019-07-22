(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The fuzzer as a filter.

    The fuzzer takes various pieces of configuration (see {{!Aux} Aux}) as
    auxiliary input, and emits a trace of actions performed (see
    {{!Trace} Trace}) as auxiliary output. *)

(** {2 Fuzzer config as an auxiliary input} *)

module Aux : sig
  type t =
    {seed: int option; o: Act_common.Output.t; config: Act_config.Fuzz.t}
  (** Type of auxiliary input to the fuzzer filter. *)
end

include
  Plumbing.Filter_types.S with type aux_i = Aux.t and type aux_o = Trace.t
