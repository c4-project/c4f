(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts
module S_spec = Act_backend.Spec

module M = struct
  type t =
    { enabled: bool [@default true] [@sexp_drop_default.equal]
    ; via: Via.t [@default Via.local] [@sexp_drop_default.equal]
    ; backends: S_spec.Set.t [@default Act_common.Spec.Set.empty] }
  [@@deriving fields, equal, make]

  (* We use a different name for the getter than the one [@@deriving fields]
     infers. *)
  let is_enabled = enabled

  let pp_enabled (f : Base.Formatter.t) : bool -> unit = function
    | true ->
        ()
    | false ->
        Fmt.any "@ (DISABLED)" f ()

  let pp =
    Fmt.(
      hbox
        (using
           (fun {via; enabled; _} -> (via, enabled))
           (pair ~sep:nop Via.pp pp_enabled)))

  let pp_summary = pp (* for now *)

  let remoteness x = Via.remoteness (via x)

  let runner x = Via.to_runner (via x)
end

include M
module With_id = Ac.Spec.Make_with_id (M)

module MS = Ac.Spec.Make (struct
  include M

  let type_name = "machine"

  module With_id = With_id
end)

include (
  MS : module type of MS with type t := M.t and module With_id := With_id )
