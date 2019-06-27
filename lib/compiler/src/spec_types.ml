(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [S_spec] is the signature common to any sort of compiler specification,
    including [With_id] pairs.

    In practice, modules implementing this will either be [Spec] or
    [Spec.With_id]. *)
module type S = sig
  type t [@@deriving equal]
  (** Opaque type of compiler specifications. *)

  val style : t -> Act_common.Id.t
  (** [style c] gets the invocation style of [c]. *)

  val emits : t -> Act_common.Id.t
  (** [emits c] gets the architecture emitted by [c]. *)

  val cmd : t -> string
  (** [cmd] gets the command used to invoke [c]. *)

  val argv : t -> string list
  (** [argv] gets any extra arguments to supply to [c]. *)
end
