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

(** Signature common to any sort of machine specification, including
    [With_id] pairs. *)
module type S = sig
  type t
  (** [t] describes a machine. *)

  type via
  (** Type of 'via' blocks. *)

  include Pretty_printer.S with type t := t

  (** {3 Compilers attached to this spec} *)

  val compilers : t -> Act_compiler.Spec.Set.t
  (** [compilers spec] gets the compiler specifications attached to [spec]. *)

  (** {3 Simulators attached to this spec} *)

  val sims : t -> Act_sim.Spec.Set.t
  (** [sims spec] gets the simulator specifications attached to [spec]. *)

  val sim : t -> id:Act_common.Id.t -> Act_sim.Spec.With_id.t Or_error.t
  (** [sim spec ~id] gets the high-level specification in [spec] for the
      simulator with identifier [id], if one exists. *)

  (** {3 Running things on this machine} *)

  val via : t -> via
  (** [via spec] gets the [via] stanza of a machine spec [spec]. *)

  val remoteness : t -> [`Remote | `Local | `Unknown]
  (** [remoteness m] returns a guess as to whether machine reference [m] is
      a reference to a remote machine. *)

  val runner : t -> (module Plumbing.Runner_types.S)
  (** [runner spec] gets a runner for the machine spec [spec]. *)
end
