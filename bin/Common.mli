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
  | `Arch of Id.t
  ]

val warn_if_not_tracking_symbols
  :  Output.t
  -> string list
  -> unit
(** [warn_if_not_tracking_symbols o c_symbols] prints a warning on [o]
    if [c_symbols] is empty.  The warning explains that, without any
    C symbols to track, act may make incorrect assumptions. *)

val get_target
  :  Lib.Config.M.t
  -> [< `Id of Id.t | `Arch of Id.t ]
  -> target Or_error.t
(** [get_target cfg target] processes a choice between compiler ID
    and architecture ID; if the input is a compiler
    ID, the compiler is retrieved from [cfg]. *)

val arch_of_target : target -> Id.t
(** [arch_of_target target] gets the architecture Id
   associated with a target (either a compiler spec or emits
   clause). *)

val asm_runner_of_target : target -> (module Asm_job.Runner) Or_error.t
(** [asm_runner_of_target target] gets the [Asm_job.Runner]
   associated with a target (either a compiler spec or emits
   clause). *)


module Compiler_chain_input : sig
  type next_mode =
    [ `Preview
    | `No_compile
    | `Compile
    ]

  type 'a t

  val file_type : 'a t -> File_type.t_or_infer
  val next      : 'a t -> next_mode -> 'a

  val create : file_type:File_type.t_or_infer -> next:(next_mode -> 'a) -> 'a t
end

val chain_with_compiler
  :  target
  -> (module Filter.S with type aux_i = 'aux_i and type aux_o = 'aux_o)
  -> (module Filter.S with type aux_i = 'aux_i Compiler_chain_input.t
                       and type aux_o = (unit option * 'aux_o)
     ) Or_error.t
(** [chain_with compiler target to_chain] finds the correct
    compiler filter for [target],
    then chains it onto [to_chain] conditional on the incoming file type.

    The second filter's auxiliary input is derived from a function that
    receives [true] if the compiler was run, and [false] otherwise. *)

module Chain_with_delitmus
    (Onto  : Filter.S)
  : Filter.S with type aux_i = (File_type.t_or_infer * (C.Filters.Output.t option -> Onto.aux_i))
              and type aux_o = (C.Filters.Output.t option * Onto.aux_o)
(** Chain a delitmusing pass onto [Onto] conditional on the incoming
   file type. *)

val chain_with_delitmus
  :  (module Filter.S with type aux_i = 'i and type aux_o = 'o)
  -> ( module Filter.S with type aux_i = (File_type.t_or_infer * (C.Filters.Output.t option -> 'i))
                        and type aux_o = (C.Filters.Output.t option * 'o)
     )
(** [chain_with_delitmus onto] is [Chain_with_delitmus], but lifted to
   first-class modules for conveniently slotting into chain builder
   pipelines. *)

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
  -> f:(Output.t -> Lib.Config.M.t -> unit Or_error.t)
  -> Standard_args.t
  -> unit
(** [lift_command ?compiler_predicate ?machine_predicate
   ?sanitiser_passes ?with_compiler_tests ~f standard_args] lifts a
   command body [f], performing common book-keeping such as loading
   and testing the configuration, creating an [Output.t], and printing
   top-level errors. *)
