(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Herd] interfaces with the Herd tool. *)

open Base

(** [t] is an opaque type representing a configured and validated Herd
    interface. *)
type t

(** [arch] tells a Herd run which architecture it should model, and,
    therefore, which model file to load. *)
type arch = C | Assembly of Config.Id.t

val create : config:Config.Herd.t -> arch:arch -> t Or_error.t
(** [create ~config ~arch] validates [config] and [arch] and, if successful,
    creates a [t]. *)

val run_direct :
     ?arch:arch
  -> ?oc:Stdio.Out_channel.t
  -> Config.Herd.t
  -> string list
  -> unit Or_error.t
(** [run_direct ?arch ?oc config argv] runs the Herd binary configured in
    [config], with the arguments in [argv] plus, if [arch] is present, any
    arguments required to effect [config]'s configuration for [arch] (like
    model overrides etc.). Any output will be sent to [oc], or standard
    output if [oc] is absent.

    Most Herd use-cases should use {{!run} run} or {{!Filter} Filter}
    instead -- this is a lower-level function intended for things like the
    `act tool` command. *)

module Filter : Utils.Filter.S with type aux_i = t and type aux_o = unit
(** We can run Herd as a filter. *)

include Sim_runner.S with type t := t
(** We can also use Herd as a simulator runner. *)
