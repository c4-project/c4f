(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A specialisation of {!Act_litmus.Header} that includes JSON
    serialisation and deserialisation. *)

type t = Constant.t Act_litmus.Header.t [@@deriving equal, yojson]

(** Loading Litmus headers from JSON. *)
include Plumbing.Loadable_types.S with type t := t

(** A filter for replacing the header of a C-mini Litmus test. *)
module Replace_filter : Plumbing.Filter_types.S
  with type aux_i = t
   and type aux_o = unit

