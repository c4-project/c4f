(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Functor for building a backend output reader. *)

module Make (B : Reader_types.Basic) : Reader_types.S
(** [Make (B)] makes a full backend output reader from the basic form [B]. *)
