(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Quickcheck generators for typesafe atomic stores *)

open Import

(** Type of assign generators. *)
module type S = sig
  type t = Fir.Assign.t [@@deriving sexp_of, quickcheck]
end

(** Generates type-safe, but arbitrary, integer assigns over the given
    environments. *)
module Int (_ : Fir.Env_types.S) (_ : Fir.Env_types.S) : S

(** Generates type-safe, but arbitrary, Boolean assigns over the given
    environments. *)
module Bool (_ : Fir.Env_types.S) (_ : Fir.Env_types.S) : S

val int : src:Fir.Env.t -> dst:Fir.Env.t -> Fir.Assign.t Q.Generator.t option
(** [int ~src ~dst] tries to build an integer assign generator safely; it
    returns [None] if no assignable integers exist in [src]. *)

val bool :
  src:Fir.Env.t -> dst:Fir.Env.t -> Fir.Assign.t Q.Generator.t option
(** [int ~src ~dst] tries to build an Bool assign generator safely; it
    returns [None] if no assignable Booleans exist in [src]. *)

val any : src:Fir.Env.t -> dst:Fir.Env.t -> Fir.Assign.t Q.Generator.t option
(** [any ~src ~dst] tries to build a generator that randomly chooses an
    integer or Boolean assign, depending on the variables available in [src].
    It is [None] if both [int ~src ~dst] and [bool ~src ~dst] are [None]. *)
