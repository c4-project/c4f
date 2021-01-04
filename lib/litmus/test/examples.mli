(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Example test data, used in various {{!Litmus} Litmus library} tests. *)

(** Example data based on the 'SBSC' litmus test. *)
module Sbsc : sig
  val header : int C4f_litmus.Header.t Lazy.t
  (** An SBSC auxiliary record. *)

  val test : (int, string) C4f_litmus.Test.Raw.t Lazy.t
  (** A (highly abstracted) SBSC test, using {{!aux} aux}. *)
end
