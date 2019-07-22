(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Fqueue *)

type elt = {name: Act_common.Id.t; payload: Sexp.t} [@@deriving sexp]

type t = elt Fqueue.t [@@deriving sexp]

let empty : t = Fqueue.empty

let add (type p) (trace : t)
    ~(action : (module Action_types.S with type Random_state.t = p))
    ~(payload : p) : t =
  let (module Action) = action in
  let name = Action.name in
  let payload = Action.Random_state.sexp_of_t payload in
  Fqueue.enqueue trace {name; payload}
