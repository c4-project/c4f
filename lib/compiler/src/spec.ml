(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Au = Act_utils

module M = struct
  type t =
    { argv: string list [@sexp.list]
    ; enabled: bool [@sexp.bool]
    ; style: Ac.Id.t
    ; emits: Ac.Id.t
    ; cmd: string }
  [@@deriving sexp, fields, equal, make]

  (* We use a different name for the getter than the one [@@deriving fields]
     infers. *)
  let is_enabled = enabled

  let pp =
    Fmt.vbox (fun f spec ->
        if not spec.enabled then Fmt.pf f "-- DISABLED --@," ;
        Au.My_format.pp_kv f "Style" Ac.Id.pp spec.style ;
        Fmt.cut f () ;
        Au.My_format.pp_kv f "Emits" Ac.Id.pp spec.emits ;
        Fmt.cut f () ;
        Au.My_format.pp_kv f "Command"
          (Fmt.list ~sep:Fmt.sp String.pp)
          (spec.cmd :: spec.argv) )

  let pp_summary =
    let facts spec =
      List.filter_opt [Option.some_if (not (enabled spec)) "(DISABLED)"]
    in
    Fmt.(using facts (hbox (list ~sep:sp string)))
end

module With_id = struct
  module W = Ac.Spec.With_id (M)
  include W

  module H = Act_utils.Inherit.Helpers (struct
    type t = W.t

    type c = M.t

    let component = spec
  end)

  let is_enabled = H.forward M.is_enabled

  let style = H.forward M.style

  let emits = H.forward M.emits

  let cmd = H.forward M.cmd

  let argv = H.forward M.argv
end

module Spec_basic = struct
  include M

  let type_name = "compiler"

  module With_id = With_id
end

module Spec = Ac.Spec.Make (Spec_basic)

include (
  Spec : module type of Spec with module With_id := Spec_basic.With_id )

include (M : module type of M with type t := M.t)
