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

open Base

(** {3 Input interfaces} *)

(** Usual input for simulator runner functors. *)
module type Basic = sig
  val machine_id : Act_common.Id.t
  (** The ID of the machine running this simulator. *)

  val spec : Spec.t
  (** The simulator spec describing how to run this simulator. *)

  (** Runner used for running the simulator, possibly remotely. *)
  module Runner : Plumbing.Runner_types.S
end

(** Shorthand for the specific type of filter a simulator runner expects. *)
module type Basic_filter =
  Plumbing.Filter_types.S with type aux_i = Arch.t and type aux_o = unit

(** Input for generating simulators that always fail with a particular error
    on use. *)
module type Basic_error = sig
  val error : Error.t
  (** The error to return on use. *)
end

(** Signature common to both [Basic] and fully instantiated ([S]) runners. *)
module type Common = sig
  val name : Act_common.Id.t
  (** [name] is the name of the simulator, as an act ID. This can differ
      from the ID under which this simulator was registered in any runner
      tables. *)

  val machine_id : Act_common.Id.t
  (** [machine_id] is the ID of the machine on which this simulator is
      running. *)

  (** The main body of the simulator runner is a filter from litmus tests to
      output files. The filter shouldn't produce any auxiliary output, and
      should accept an architecture as its input. *)
  module Filter : Basic_filter

  (** Simulator runners must be able to load back their output into the
      simulator output format. *)
  module Reader : Reader_intf.Basic
end

(** Basic interface for simulator runners constructed from filters.

    Such runners consist of two stages: a filter that runs the simulator on
    a litmus test to produce an opaque file output, and a loader that reads
    that output back in as generic simulator output. *)
module type Basic_from_filter = sig
  include Common
end

(** {3 Output interfaces} *)

(** Main interface for simulator runners. *)
module type S = sig
  include Common

  val run :
       Arch.t
    -> input_path:Fpath.t
    -> output_path:Fpath.t
    -> Output.t Or_error.t
  (** [run ctx ~input_path ~output_path] runs the simulator on [input_path],
      using [ctx] as context, outputs to [output_path], and then tries to
      load back the results as generic simulator output. *)
end
