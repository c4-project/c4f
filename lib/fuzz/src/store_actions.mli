(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** C fuzzer: actions that generate store instructions. *)

(** {2 Random state} *)

module Random_state : sig
  type t

  val make : store:Act_c_mini.Atomic_store.t -> path:Path.Program.t -> t

  val store : t -> Act_c_mini.Atomic_store.t

  val path : t -> Path.Program.t
end

(** {2 Functors} *)

(** [Make (B)] makes a store action given the basic configuration in [B]. *)
module Make (B : sig
  val name : Act_common.Id.t
  (** The name of this store action. *)

  val default_weight : int
  (** The default weight of this store action. *)

  val forbid_already_written : bool
  (** If true, only allow stores to variables that are known not to already
      have writes. This can help avoid combinatorial explosions. *)

  val dst_type : Act_c_mini.Type.Basic.t
  (** The expected basic type of destination variables. This must line up
      with the generator. *)

  (** The generator this store action uses to create stores. *)
  module Quickcheck
      (Src : Act_c_mini.Env_types.S)
      (Dst : Act_c_mini.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t := Act_c_mini.Atomic_store.t
end) : Action_types.S with type Payload.t = Random_state.t

(** {2 Pre-made modules} *)

module Int : Action_types.S with type Payload.t = Random_state.t
(** [Int] is a fuzzer action that generates a random atomic-int store
    instruction. *)
