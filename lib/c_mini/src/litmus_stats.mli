(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Extraction of various statistics from a C litmus test. *)

open Base

module Statset : sig
  (** Type of statistic outputs. *)
  type t =
    { threads: int
    ; returns: int
    ; literal_bools: int
    ; expr_atomics: int Map.M(Atomic_class).t
    ; expr_mos: int Map.M(Mem_order).t
    ; stm_atomics: int Map.M(Atomic_class).t
    ; stm_mos: int Map.M(Mem_order).t }

  (** We can pretty-print statistic outputs. *)
  include Pretty_printer.S with type t := t
end

val scrape : Litmus.Test.t -> Statset.t
(** [scrape test] extracts statistics from [test]. *)

(** Lifting of [scrape] to a filter from litmus test files to statistic
    outputs. *)
module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit
