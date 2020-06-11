(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Mini = Act_fir

module type S = sig
  val tid : int

  val when_local :
       'a
    -> over:('a -> Act_common.C_id.t)
    -> f:('a -> 'a Or_error.t)
    -> 'a Or_error.t

  val when_global :
       'a
    -> over:('a -> Act_common.C_id.t)
    -> f:('a -> 'a Or_error.t)
    -> 'a Or_error.t
end

module Make (B : sig
  val tid : int

  val locals : Set.M(Act_common.C_id).t
end) : S = struct
  let tid = B.tid

  let is_local : Act_common.C_id.t -> bool = Set.mem B.locals

  let when_local (v : 'a) ~(over : 'a -> Act_common.C_id.t)
      ~(f : 'a -> 'a Or_error.t) : 'a Or_error.t =
    if is_local (over v) then f v else Or_error.return v

  let when_global (v : 'a) ~(over : 'a -> Act_common.C_id.t)
      ~(f : 'a -> 'a Or_error.t) : 'a Or_error.t =
    if is_local (over v) then Or_error.return v else f v
end
