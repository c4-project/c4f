(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** High-level front-end for assembly translation jobs

    [Job] specifies a signature, [Runner], that describes a module that
    takes an act job specification (of type [t]) and, on success,
    produces output (of type [output]).  Such [Runner]s abstract over
    all of the I/O plumbing and other infrastructure needed to do
    the jobs. *)

open Base
include module type of Asm_job_intf

(** [t] is a description of a single-file job. *)
type 'cfg t

(** [make ?config ?passes ?symbols ()] makes a job description. *)
val make
  :  ?config:'cfg
  -> ?passes:Config.Sanitiser_pass.Set.t
  -> ?symbols:string list
  -> unit
  -> 'cfg t

module Explain_config : sig
  module Format : sig
    (** [t] is an enumeration of output formats for explain jobs. *)
    type t =
      | Assembly (** Terse, but as close to parseable assembly as possible *)
      | Detailed (** More details than [Assembly], but verbose and free-form *)
    [@@deriving equal]

    (** [default] gets the default output format. *)
    val default : t
  end

  type t [@@deriving equal, sexp]

  (** [make ?format ()] builds an [Explain_config] with the given
     parameters. *)
  val make : ?format:Format.t -> unit -> t

  (** [default] gets the default explainer job configuration. *)
  val default : t
end

(** The output of a single-file job. *)
module Output : sig
  type t

  (** [symbol_map o] returns the mapping from pre-compiler
      symbols (given in the input [t]) to mangled
      assembly symbols for the job output [o]. *)
  val symbol_map : t -> (string, string) List.Assoc.t

  (** [warn f o] prints any warnings attached to output [o] on
      pretty-print formatter [f]. *)
  val warn : t Fmt.t
end

(** Signature of job runners. *)
module type Runner = sig
  type const [@@deriving sexp]

  include
    Gen_runner
    with type 'cfg inp := 'cfg t
     and type aux := Output.t
     and type lcfg := const Litmusifier.Config.t
     and type ecfg := Explain_config.t
end

(** [Make_runner] makes a [Runner] from a [Runner_deps] module. *)
module Make_runner (R : Runner_deps) : Runner with type const = R.Src_lang.Constant.t

(** {2 First-class wrappers for getting job runners} *)

(** [get_litmusify_sexp Runner] is [Runner.Litmusify], but with the
    input type altered slightly so that the constants inside any
    litmus postconditions are expected to be S-expressions, and
    unmarshalled into the appropriate language at run-time. *)
val get_litmusify_sexp
  :  (module Runner)
  -> (module Utils.Filter.S
        with type aux_i = Sexp.t Litmusifier.Config.t t
         and type aux_o = Output.t)

(** [get_explain Runner] is [Runner.Explain]. *)
val get_explain
  :  (module Runner)
  -> (module Utils.Filter.S
        with type aux_i = Explain_config.t t
         and type aux_o = Output.t)
