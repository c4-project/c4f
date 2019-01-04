(* This file is part of 'act'.

Copyright (c) 2018, 2019 by Matt Windsor

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

open Stdio
open Base

module type Basic = sig
  type aux_i
  (** Type of any auxiliary state consumed by this filter. *)

  type aux_o
  (** Type of any auxiliary state built by this filter. *)


  val run
    :  aux_i
    -> Io.In_source.t
    -> In_channel.t
    -> Io.Out_sink.t
    -> Out_channel.t
    -> aux_o Or_error.t
  ;;
end

module type Basic_in_file_only = sig
  type aux_i
  (** Type of any auxiliary state consumed by this filter. *)

  type aux_o
  (** Type of any auxiliary state built by this filter. *)

  val run
    :  aux_i
    -> Fpath.t
    -> Io.Out_sink.t
    -> Out_channel.t
    -> aux_o Or_error.t
  ;;
end

module type Basic_files_only = sig
  type aux_i
  (** Type of any auxiliary state consumed by this filter. *)

  type aux_o
  (** Type of any auxiliary state built by this filter. *)

  val run
    :  aux_i
    -> infile:Fpath.t
    -> outfile:Fpath.t
    -> aux_o Or_error.t
  ;;
end

module type S = sig
  type aux_i
  (** Type of any auxiliary state consumed by this filter. *)

  type aux_o
  (** Type of any auxiliary state built by this filter. *)

  val run
    :  aux_i
    -> Io.In_source.t
    -> Io.Out_sink.t
    -> aux_o Or_error.t
  (** [run aux source sink] runs this filter on [source], outputs to
     [sink], reads and returns any auxiliary state on success. *)

  val run_from_fpaths
    :  aux_i
    -> infile:Fpath.t option
    -> outfile:Fpath.t option
    -> aux_o Or_error.t
  (** [run_from_fpaths aux ~infile ~outfile] runs this filter on
     [infile] (if [None], use stdin), outputs to [outfile] (if [None],
     use stdout), and returns any auxiliary state on success. *)

  val run_from_string_paths
    :  aux_i
    -> infile:string option
    -> outfile:string option
    -> aux_o Or_error.t
    (** [run_from_string_paths aux ~infile ~outfile] runs this filter
       on [infile] (if [None], use stdin), outputs to [outfile] (if
       [None], use stdout), and returns any auxiliary state on
       success. *)
end

(** Signature of inputs needed to build a conditional chain. *)
module type Basic_chain_conditional = sig
  module First  : S (** The first filter. *)
  module Second : S (** The second filter. *)

  type aux_i_combi  (** Combined auxiliary input. *)
  type aux_i_single (** Auxiliary input used when not chaining. *)

  val select
    :  aux_i_combi
    -> Io.In_source.t
    -> Io.Out_sink.t
    -> [`Both of (First.aux_i * Second.aux_i) | `One of aux_i_single]
  (** [condition a_aux b_aux src snk] should return [true] when the optional
     filter should be run (which filter this is depends on the
     functor). *)
end

(** Signature of inputs needed to build a conditional chain with
    the first filter being conditional. *)
module type Basic_chain_conditional_first = sig
  module Second : S (** The second filter. *)
  include Basic_chain_conditional
    with module Second := Second
     and type aux_i_single := Second.aux_i
end

(** Signature of inputs needed to build a conditional chain with
    the second filter being conditional. *)
module type Basic_chain_conditional_second = sig
  module First : S (** The first filter. *)
  include Basic_chain_conditional
    with module First := First
     and type aux_i_single := First.aux_i
end
