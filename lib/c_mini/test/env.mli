(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Tests, and test data, for {{!Act_c_mini.Env} Env}. *)

open Base

(** {2 Test typing environments}

    These (lazily-evaluated) values contain simple pre-populated test
    environments that can be used for expects tests, as well as Quickcheck
    tests that depend on a well-formed environment. *)

val test_env : Act_c_mini.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env] is a typing environment used for testing the various
    environment-sensitive operations. *)

val test_env_atomic_ptrs_only :
  Act_c_mini.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env_atomic_ptrs_only] is a typing environment that contains only
    variables of type 'atomic_int*' and 'atomic_bool*'. *)

val test_env_scalars_only :
  Act_c_mini.Type.t Map.M(Act_common.C_id).t Lazy.t
(** [test_env_atomic_ptrs_only] is a typing environment that contains only
    variables of scalar (but possibly atomic) type. *)

(** {3 Test typing environment modules} *)

val test_env_mod : (module Act_c_mini.Env_types.S) Lazy.t
(** {{!test_env} test_env} packaged as a first-class module. *)

val test_env_atomic_ptrs_only_mod : (module Act_c_mini.Env_types.S) Lazy.t
(** {{!test_env_atomic_ptrs_only} test_env_atomic_ptrs_only} packaged as a
    first-class module. *)

val test_env_scalars_only_mod : (module Act_c_mini.Env_types.S) Lazy.t
(** {{!test_env_scalars_only} test_env_scalars_only} packaged as a
    first-class module. *)

val empty_env_mod : (module Act_c_mini.Env_types.S) Lazy.t
(** A first-class module containing a completely empty environment. *)

(** {2 Test known-value environment modules} *)

val det_known_value_mod :
  (module Act_c_mini.Env_types.S_with_known_values) Lazy.t
(** [det_known_value_mod] expands {{!test_env_mod} test_env_mod} to contain
    singleton known values for all non-pointer variables. *)
