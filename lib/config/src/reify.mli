(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Reifying a global config structure into an abstract syntax tree. *)

(** {1 Machine config} *)

module Machines : sig
  val reify : Act_machine.Spec.t Act_common.Spec.Set.t -> Ast.t
  (** [reify machines] creates an abstract syntax tree that represents a
      possible configuration file fragment that could be parsed to yield the
      machine specs [machines]. *)

  include
    Base.Pretty_printer.S
      with type t := Act_machine.Spec.t Act_common.Spec.Set.t
  (** We can pretty-print machine configuration by reifying them. *)
end

(** {1 The global config}*)

val reify : Global.t -> Ast.t
(** [reify global] creates an abstract syntax tree that represents a possible
    configuration file that could be parsed to yield [global]. *)

include Base.Pretty_printer.S with type t := Global.t
(** We can pretty-print global config structures by reifying them. *)
