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

(** [Herd] interfaces with the Herd tool. *)

open Core
open Utils

(** [Config] describes configuration needed to find and execute
    Herd. *)
module Config : sig
  (** [t] is the type of Herd configuration. *)
  type t =
    { cmd        : string
    ; c_model    : string option
    ; asm_models : (string list, string) List.Assoc.t
    } [@@deriving sexp]
  ;;

  val create
    :  ?cmd:string
    -> ?c_model:string
    -> ?asm_models:((string list, string) List.Assoc.t)
    -> unit
    -> t
  ;;
end

(** [t] is an opaque type representing a configured and validated
    Herd interface. *)
type t

(** [arch] tells a Herd run which architecture it should model, and,
   therefore, which model file to load. *)
type arch =
  | C
  | Assembly of string list
;;

(** [create ~config ~arch] validates [config] and [arch] and, if
   successful, creates a [t]. *)
val create : config:Config.t -> arch:arch -> t Or_error.t

(** [run ctx ~path ~sink] runs Herd (represented by [ctx]) on the
   Litmus test at [path] using the model and other configuration for
   architecture [arch].  It outputs the results to [sink], but doesn't
   analyse them. *)
val run
  :  t
  -> path:Fpath.t
  -> sink:Io.Out_sink.t
  -> unit Or_error.t
;;

module Filter : Filter.S with type aux_i = t
                          and type aux_o = unit
  (** [run], but bundled up as a [Filter] for use in chains. *)

(** [run_and_load_results ctx ~input_path ~output_path] behaves
   like [run], but then reads [output_path] back in as a
   [Herd_output.t].  This requires [output_path] to point to a file,
   rather than being any [Out_sink.t]. *)
val run_and_load_results
  :  t
  -> input_path:Fpath.t
  -> output_path:Fpath.t
  -> Herd_output.t Or_error.t
;;
