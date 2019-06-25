(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures of things that can be (de)serialised using Yojson. *)

open Base

(** Signature of things that can be serialised to (safe Yo-)json. *)
module type To = sig
  type t

  val to_yojson : t -> Yojson.Safe.t
end

(** Signature of things that can be deserialised from (safe Yo-)json.

    Deliberately aligned with the API of [ppx_deriving_yojson]. *)
module type Of = sig
  type t

  val of_yojson : Yojson.Safe.t -> (t, string) Result.t
end

(** Signature of things that can be serialised to, and deserialised from,
    (safe Yo-)json. *)
module type S = sig
  type t

  include To with type t := t

  include Of with type t := t
end
