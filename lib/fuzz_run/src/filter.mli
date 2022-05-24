(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** The fuzzer, packaged up as filters.

    Each filter takes in a Litmus test, and outputs another Litmus test; the
    auxiliary input and output concerns things like traces, seeds, and
    verbose output. *)

open Base
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

    val make : ?o:Common.Output.t -> C4f_fuzz.Trace.t -> t
    (** [make ?o trace] makes an auxiliary input for the replay filter given
        optional outputter [o] and trace-to-replay [trace]. *)
  end
end

val run_randomised :
     Plumbing.Input.t
  -> Plumbing.Output.t
  -> aux:Aux.Randomised.t
  -> Aux.Output.t Or_error.t
(** [run_randomised input output ~aux] runs the randomised runner over a
    litmus test coming from [input], outputting the new test to [output], and
    pulling various bits of side-data from [aux]. *)

val run_replay :
     Plumbing.Input.t
  -> Plumbing.Output.t
  -> aux:Aux.Replay.t
  -> Aux.Output.t Or_error.t
(** [run_replay input output ~aux] replays a trace runner over a litmus test
    coming from [input], outputting the new test to [output], and pulling the
    trace and various bits of side-data from [aux]. *)
