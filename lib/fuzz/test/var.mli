(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Test data, used in this and other tests. *)
module Test_data : sig
  val test_map : Act_fuzz.Var.Map.t Lazy.t
  (** [test_map] evaluates to an example variable map. *)
end
