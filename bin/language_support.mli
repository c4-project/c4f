(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Language lookup and module building support

    The various top-level act commands need to invoke the
   language-independent bits of act (in the [Lib] module) using the
   appropriate language-dependent bits.  This module works out which
   language is needed by looking at the 'emits' clause of a compiler
   spec, and hooks up the correct language-dependent modules. *)

open Core
open Lib

val asm_runner_from_arch : Id.t -> (module Asm_job.Runner) Or_error.t
(** [asm_runner_from_arch arch] generates an assembly job runner
   from an architecture ID [arch]. *)

(** Compiler resolver that uses this module's built-in compiler table
    to look up compilers. *)
module Resolve_compiler
  : Compiler.S_resolver
    with type spec = Compiler.Spec.With_id.t
     and type 'a chain_input = 'a Compiler.Chain_input.t
;;

(** Compiler resolver that uses this module's built-in compiler table
    to look up compilers from targets, filling in a dummy compiler
    if the target doesn't mention a compiler. *)
module Resolve_compiler_from_target
  : Compiler.S_resolver
    with type spec = Compiler.Target.t
     and type 'a chain_input = 'a Compiler.Chain_input.t
;;

(** [load_and_process_config ?compiler_predicate ?machine_predicate
   ?sanitiser_passes ?with_compiler_tests path] loads the config file
   at [path] and optionally tests all machines and compilers therein.

    If [compiler_predicate] (a Blang expression) is present,
   only compilers satisfying it (on enabled machines) will be
   accepted.

    If [machine_predicate] (also a Blang expression) is present, only
   machines satisfying it will be enabled.

    If [sanitiser_passes] (another Blang expression) is present, the
    sanitiser pass override in the final config will be set to
    the result of evaluating that expression.

    If [with_compiler_tests] is absent, or present and true, compilers
   will be tested for reachability. *)
val load_and_process_config
  :  ?compiler_predicate:Compiler.Property.t Blang.t
  -> ?machine_predicate:Machine.Property.t Blang.t
  -> ?sanitiser_passes:Sanitiser_pass.Selector.t Blang.t
  -> ?with_compiler_tests:bool (* default true *)
  -> Fpath.t
  -> Config.M.t Or_error.t
;;
