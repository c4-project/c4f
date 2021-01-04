(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Test data, used in this and other tests. *)
module Test_data : sig
  val test_map : C4f_fuzz.Var.Map.t Lazy.t
  (** [test_map] evaluates to an example variable map. *)
end
