(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Various lazily-evaluated tables used to source information about fuzzer
    configurables. *)

open Base
open Import

(** {1 Tables} *)

val param_map : Param_spec.Int.t Map.M(Common.Id).t Lazy.t
(** [param_map] lazily evaluates to a map from integer parameter IDs to their
    specifications, including their default values. *)

val flag_map : Param_spec.Bool.t Map.M(Common.Id).t Lazy.t
(** [flag_map] lazily evaluates to a map from Boolean flag IDs to their
    specifications, including their default values. *)

(** {1 Parameter keys} *)

val action_cap_lower_param : Common.Id.t
(** [action_cap_lower_param] names the parameter that specifies the number of
    actions that should always be applied by the fuzzer, before considering
    extra actions. *)

val action_cap_upper_param : Common.Id.t
(** [action_cap_upper_param] names the parameter that specifies the maximum
    number of actions that should be applied by the fuzzer, including extra
    actions. *)

val action_deck_size_param : Common.Id.t
(** [action_deck_size_param] names the parameter that specifies the maximum
    number of actions that should be kept in the deck. *)

val thread_cap_param : Common.Id.t
(** [thread_cap_param] names the parameter that specifies the maximum number
    of threads that should exist in a fuzzed test. *)

(** {1 Flag keys} *)

val make_global_flag : Common.Id.t
(** [make_global_flag] names the flag that decides whether variable creation
    actions should make globals if true (and locals if false). This flag is
    usually stochastic, to let such actions pick globals sometimes and locals
    at other times. *)

val wrap_early_out_flag : Common.Id.t
(** [wrap_early_out_flag] names the flag that decides whether load-bearing
    early-out actions should wrap their early-outs in an 'if true', to
    confuse the compiler a little. This flag is usually stochastic. *)

val extra_action_flag : Common.Id.t
(** [extra_action_flag] names the flag that decides, each time the randomised
    fuzzer runner considers picking an extra action, whether it does so or
    gives up. This flag is usually stochastic, so as to form a sort-of
    geometric distribution. *)

(** {2 Recommendation queue} *)

val accept_recommendation_flag : Common.Id.t
(** [accept_recommendation_flag] names the flag that decides whether the
    randomised fuzzer runner accepts action recommendations. This flag is
    usually stochastic. *)

val use_recommendation_flag : Common.Id.t
(** [use_recommendation_flag] names the flag that decides whether the
    randomised fuzzer runner picks actions from the recommendation queue over
    those in its weighted deck. This flag is usually stochastic. *)

(** {2 Unsafe flags} *)

val unsafe_weaken_orders_flag : Common.Id.t
(** [unsafe_weaken_orders_flag] names the unsafe flag that, when true, lets
    memory-order modifying actions weaken memory orders. *)
