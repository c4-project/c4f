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

(** Glue code common to all top-level commands *)

open Core_kernel
open Lib
open Utils

type target =
  [ `Spec of Compiler.Spec.With_id.t
  | `Arch of string list
  ]

type file_type =
  [`Assembly | `C | `Infer]

val warn_if_not_tracking_symbols
  :  Output.t
  -> string list
  -> unit
(** [warn_if_not_tracking_symbols o c_symbols] prints a warning on [o]
    if [c_symbols] is empty.  The warning explains that, without any
    C symbols to track, act may make incorrect assumptions. *)

val decide_if_c : Fpath.t option -> [> `C | `Infer] -> bool
(** [decide_if_c infile filetype] decides whether [infile] is a C
    file---from its extension if [filetype] is [`Infer], or
    by whether or not [filetype] is [`C]. *)

val get_target
  :  Config.M.t
  -> [< `Id of Id.t | `Arch of string list ]
  -> target Or_error.t
(** [get_target cfg target] processes a choice between compiler ID
    and architecture (emits clause); if the input is a compiler
    ID, the compiler is retrieved from [cfg]. *)

val arch_of_target : target -> string list
(** [arch_of_target target] gets the architecture (emits clause)
   associated with a target (either a compiler spec or emits
   clause). *)

val runner_of_target : target -> (module Asm_job.Runner) Or_error.t
(** [runner_of_target target] gets the [Asm_job.Runner]
   associated with a target (either a compiler spec or emits
   clause). *)

val chain_with_compiler
  :  target
  -> file_type
  -> ( module Filter.S with type aux_i = 'i and type aux_o = 'o )
  -> ( module Filter.S with type aux_i = (unit * 'i)
                        and type aux_o = (unit option * 'o)
     ) Or_error.t
(** [chain_with compiler target file_type to_chain] finds the correct
    compiler filter for [target] (or none, depending on [file_type]),
    then chains it onto [to_chain]. *)

val ensure_spec
  :  [> `Spec of Compiler.Spec.With_id.t]
  -> Compiler.Spec.With_id.t Or_error.t
(** [ensure_spec maybe_spec] checks that [maybe_spec] is, indeed,
    a spec. *)

val lift_command
  :  ?compiler_predicate:Compiler.Property.t Blang.t
  -> ?machine_predicate:Machine.Property.t Blang.t
  -> ?sanitiser_passes:Sanitiser_pass.Selector.t Blang.t
  -> ?with_compiler_tests:bool (* default true *)
  -> f:(Output.t -> Config.M.t -> unit Or_error.t)
  -> Standard_args.t
  -> unit
(** [lift_command ?compiler_predicate ?machine_predicate
   ?sanitiser_passes ?with_compiler_tests ~f standard_args] lifts a
   command body [f], performing common book-keeping such as loading
   and testing the configuration, creating an [Output.t], and printing
   top-level errors. *)
