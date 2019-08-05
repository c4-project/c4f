(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Delitmus: main drivers.

    These modules actually 'do' the delitmusifying process: they take a
    validated C litmus AST and output a pair of C program and auxiliary
    output file. *)

open Base

(** Base signature for delitmus drivers *)
module type S = sig
  val run : Act_c_mini.Litmus.Test.t -> Output.t Or_error.t
end

module Vars_as_globals : S
(** Driver for the 'vars as globals' flavour of delitmus. It moves all
    variables into the global scope, changing all global references from
    pointers to values. *)

module Vars_as_parameters : S
(** Driver for the 'vars as parameters' flavour of delitmus. It moves all
    variables into the function parameter list, changing all local
    references from values to pointers. *)
