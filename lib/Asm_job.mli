(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** High-level front-end for assembly translation jobs

    [Job] specifies a signature, [Runner], that describes a module that
    takes an act job specification (of type [t]) and, on success,
    produces output (of type [output]).  Such [Runner]s abstract over
    all of the I/O plumbing and other infrastructure needed to do
    the jobs. *)

open Core
open Utils

(** [t] is a description of a single-file job. *)
type t =
  { inp     : Io.In_source.t
  ; outp    : Io.Out_sink.t
  ; passes  : Sanitiser_pass.Set.t
  ; symbols : string list
  }
;;

(** [output] is the output of a single-file job. *)
type output

(** [symbol_map o] returns the mapping from pre-compiler
    symbols (given in the input [t]) to mangled
    assembly symbols for the job output [o]. *)
val symbol_map : output -> (string, string) List.Assoc.t

(** [warn o f] prints any warnings attached to output [o] on
   pretty-print formatter [f]. *)
val warn : output -> Format.formatter -> unit

(** [Runner] is the signature of job runners. *)
module type Runner = sig
  (** [litmusify] runs a litmusify job. *)
  val litmusify : t -> output Or_error.t

  (** [explain] runs an explain job. *)
  val explain : t -> output Or_error.t
end

(** [Runner_deps] is a signature bringing together the modules we
   need to be able to run single-file jobs. *)
module type Runner_deps = sig
  module Frontend : LangFrontend.Intf
  module Litmus : Litmus.Intf
  module Multi_sanitiser
    : Sanitiser.S with type statement = Litmus.Lang.Statement.t
                   and type sym = Litmus.Lang.Symbol.t
                   and type 'a Program_container.t = 'a list
  ;;
  module Single_sanitiser
    : Sanitiser.S with type statement = Litmus.Lang.Statement.t
                   and type sym = Litmus.Lang.Symbol.t
                   and type 'a Program_container.t = 'a
  ;;
  module Explainer
    : Explainer.S with type statement = Litmus.Lang.Statement.t
  ;;

  val final_convert : Litmus.Lang.Statement.t list -> Litmus.Lang.Statement.t list

  val statements : Frontend.ast -> Litmus.Lang.Statement.t list
end

(** [Make_runner] makes a [Runner] from a [Runner_deps] module. *)
module Make_runner : functor (R : Runner_deps) -> Runner
