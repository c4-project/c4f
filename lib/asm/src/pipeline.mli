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

(** Functions and types for building assembly job pipelines that take in
    C/Litmus, C, and assembly files, performing the necessary
    transformations. *)

(* TODO(@MattWindsor91): in the medium term, the plan is to eliminate these
   pipelines and replace them with individual act commands composed into
   shell scripts. This is far-off at time of writing, and so this module
   exists to try and declutter the existing approach. *)

module Ac = Act_common
module Pb = Plumbing

module Input : sig
  (** Opaque type of assembly pipeline input. *)
  type 'job t

  (** {3 Constructors} *)

  val make :
       file_type:Ac.File_type.t
    -> job_input:(   Act_c.Filters.Output.t Pb.Chain_context.t
                  -> 'job Job.t Act_compiler.Instance.Chain_input.t)
    -> 'job t

  (** {3 Accessors} *)

  val file_type : _ t t -> Ac.File_type.t

  val job_input :
       'job t
    -> Act_c.Filters.Output.t Pb.Chain_context.t
    -> 'job Job.t Act_compiler.Instance.Chain_input.t
end

module Output : sig
  (** Opaque type of assembly pipeline output. *)
  type t

  (** {3 Constructors} *)

  val make :
    ?c_output:Act_c.Filters.Output.t -> job_output:Job.Output.t -> unit -> t

  (** {3 Accessors} *)

  val c_output : t -> Act_c.Filters.Output.t option

  val job_output : t -> Job.Output.t
end

module type S = sig
  type cfg

  include
    Pb.Filter_types.S
    with type aux_i = cfg Input.t
     and type aux_o = Output.t
end

val make :
     (module Act_compiler.Resolver.S with type spec = 'spec)
  -> (module Runner_intf.S with type cfg = 'c)
  -> 'spec
  -> (module S with type cfg = 'c) Or_error.t
