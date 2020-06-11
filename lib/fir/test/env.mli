(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests, and test data, for {{!Act_fir.Env} Env}. *)

open Base

(** {2 Test typing environments}

    These (lazily-evaluated) values contain simple pre-populated test
    environments that can be used for expects tests, as well as Quickcheck
    tests that depend on a well-formed environment. *)

val test_typing : Act_fir.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env] is a typing environment used for testing the various
    environment-sensitive operations. *)

val test_typing_atomic_ptrs_only :
  Act_fir.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env_atomic_ptrs_only] is a typing environment that contains only
    variables of type 'atomic_int*' and 'atomic_bool*'. *)

val test_typing_scalars_only :
  Act_fir.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env_atomic_ptrs_only] is a typing environment that contains only
    variables of scalar (but possibly atomic) type. *)

(** {3 Test typing environment modules}

    Each of these contain deterministic singleton known values for all
    non-pointer variables. *)

val test_env : Act_fir.Env.t Lazy.t
(** {!test_typing} with known-values added. *)

val test_env_atomic_ptrs_only : Act_fir.Env.t Lazy.t
(** {!test_typing_atomic_ptrs_only} with known-values added. *)

val test_env_scalars_only : Act_fir.Env.t Lazy.t
(** {!test_typing_scalars_only} with known-values added. *)

val empty_env : Act_fir.Env.t Lazy.t
(** A completely empty environment. *)
