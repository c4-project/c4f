(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Extraction of various statistics from a FIR litmus test. *)

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

val scrape_expr : Expression.t -> Statset.t
(** [scrape_expr e] extracts partial statistics from the expression [e]. *)

val scrape : Litmus.Test.t -> Statset.t
(** [scrape test] extracts statistics from [test]. *)
