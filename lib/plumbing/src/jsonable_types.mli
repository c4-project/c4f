(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures of things that can be (de)serialised using Yojson. *)

(** Signature of things that can be serialised to (safe Yo-)json. *)
module type To = sig
  type t [@@deriving yojson_of]
end

(** Signature of things that can be deserialised from (safe Yo-)json. *)
module type Of = sig
  type t [@@deriving of_yojson]
end

(** Signature of things that can be serialised to, and deserialised from,
    (safe Yo-)json. *)
module type S = sig
  type t [@@deriving yojson]
end
