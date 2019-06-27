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

module Forward_spec
    (Outer : Equal.S)
    (Inner : Spec_types.S)
    (Forwarding : Au.Inherit.S with type c := Inner.t and type t := Outer.t) :
  Spec_types.S with type t := Outer.t = struct
  module H = Act_utils.Inherit.Helpers (struct
    type t = Outer.t

    type c = Inner.t

    include Forwarding
  end)

  let equal = Outer.equal

  let style = H.forward Inner.style

  let emits = H.forward Inner.emits

  let cmd = H.forward Inner.cmd

  let argv = H.forward Inner.argv
end

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
          (spec.cmd :: spec.argv))

  let pp_summary =
    let facts spec =
      List.filter_opt [Option.some_if (not (enabled spec)) "(DISABLED)"]
    in
    Fmt.(using facts (hbox (list ~sep:sp string)))
end

module With_id = struct
  module W = Ac.Spec.With_id (M)
  include W

  include Forward_spec (W) (M)
            (struct
              let component = W.spec
            end)
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
