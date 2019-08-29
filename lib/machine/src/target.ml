(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module C_spec = Act_compiler.Spec
module Cq_spec = Qualified.Compiler

type 'compiler t = Cc of 'compiler | Arch of Act_common.Id.t

module T = Travesty.Bi_traversable.Make1_left (struct
  type nonrec 'compiler t = 'compiler t

  type right = Act_common.Id.t

  module On_monad (M : Monad.S) = struct
    let bi_map_m (type a b) (target : a t) ~(left : a -> b M.t)
        ~(right : Act_common.Id.t -> Act_common.Id.t M.t) : b t M.t =
      match target with
      | Cc x ->
          M.map ~f:(fun x' -> Cc x') (left x)
      | Arch x ->
          M.map ~f:(fun x' -> Arch x') (right x)
  end
end)

include (
  T :
    Travesty.Bi_traversable_types.S1_left
      with type 'compiler t := 'compiler t
       and type right = Act_common.Id.t )

let arch : Cq_spec.t t -> Ac.Id.t = function
  | Cc spec ->
      C_spec.With_id.emits (Cq_spec.c_spec spec)
  | Arch arch ->
      arch

let ensure_spec : 'spec t -> 'spec Or_error.t = function
  | Cc spec ->
      Or_error.return spec
  | Arch _ ->
      Or_error.error_string "Expected a compiler; got an architecture."
