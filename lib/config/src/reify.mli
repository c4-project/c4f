(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying a global config structure into an abstract syntax tree. *)

(** {1 The global config}*)

val reify : Global.t -> Ast.t
(** [reify global] creates an abstract syntax tree that represents a possible
    configuration file that could be parsed to yield [global]. *)

(** We can pretty-print global config structures by reifying them. *)
include Base.Pretty_printer.S with type t := Global.t
