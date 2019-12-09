(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Part of the delitmus aux record that tracks function movements from Litmus
    to C. *)

open Base

(** {2 Information about function parameters. *)

module Record : sig
  type t [@@deriving yojson, equal]

  val make : ?is_thread_body: bool -> c_id:Act_common.C_id.t -> unit -> t

  val c_id : t -> Act_common.C_id.t
  val is_thread_body : t -> bool
end

(** {1 JSON encodable shorthand for a function map} *)

type t = Record.t Map.M(Act_common.C_id).t [@@deriving equal, yojson]

(** {1 Accessors} *)

val num_threads : t -> int
(** num_threads [map] counts the number of threads in [map]. *)
