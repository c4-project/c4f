(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Part of the delitmus aux record that tracks function movements from
    Litmus to C. *)

open Base
open Import

(** {1 Information about functions} *)

module Record : sig
  (** Type of function records. *)
  type t = {tid: int option [@default None]; c_id: Common.C_id.t}
  [@@deriving yojson, equal, accessors]

  val is_thread_body : t -> bool
  (** [is_thread_body r] is true if [r] has an assigned thread ID. *)
end

(** {1 JSON encodable shorthand for a function map} *)

type t = Record.t Map.M(Common.C_id).t [@@deriving equal, yojson]

(** {1 Accessors} *)

val num_threads : t -> int
(** num_threads [map] counts the number of threads in [map]. *)
