(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various lazily-evaluated tables used to source information about fuzzer
    configurables. *)

open Base

(** {1 Tables} *)

val param_map : Param_spec.Int.t Map.M(Act_common.Id).t Lazy.t
(** [param_map] lazily evaluates to a map from integer parameter IDs to their
    specifications, including their default values. *)

val flag_map : Param_spec.Bool.t Map.M(Act_common.Id).t Lazy.t
(** [flag_map] lazily evaluates to a map from Boolean flag IDs to their
    specifications, including their default values. *)

(** {1 Flag keys} *)

val forbid_already_written_flag : Act_common.Id.t
(** [forbid_already_written_flag] names the flag that, when true, stops store
    actions from considering variables with existing writes. *)

val make_global_flag : Act_common.Id.t
(** [make_global_flag] names the flag that decides whether variable creation
    actions should make globals if true (and locals if false). This flag is
    usually stochastic, to let such actions pick globals sometimes and locals
    at other times. *)

(** {2 Unsafe flags} *)

val unsafe_weaken_orders_flag : Act_common.Id.t
(** [unsafe_weaken_orders_flag] names the unsafe flag that, when true, lets
    memory-order modifying actions weaken memory orders. *)
