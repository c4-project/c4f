(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Test helpers for storelike action tests *)

module Test_common : sig
  val prepare_fuzzer_state : unit -> unit C4f_fuzz.State.Monad.t
  (** [prepare_fuzzer_state] populates the fuzzer state with some test data. *)
end
