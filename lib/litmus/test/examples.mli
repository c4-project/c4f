(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Example test data, used in various {{!Litmus} Litmus library} tests. *)

(** Example data based on the 'SBSC' litmus test. *)
module Sbsc : sig
  val header : int Act_litmus.Header.t Lazy.t
  (** An SBSC auxiliary record. *)

  val test : (int, string) Act_litmus.Test.Raw.t Lazy.t
  (** A (highly abstracted) SBSC test, using {{!aux} aux}. *)
end
