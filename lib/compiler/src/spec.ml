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
    (Forwarding : Au.Inherit_types.S
                    with type c := Inner.t
                     and type t := Outer.t) :
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

  let cmd_line (s : t) : string list = s.cmd :: s.argv

  let pp =
    Fmt.(
      record
        [ field "Enabled" is_enabled bool
        ; field "Style" style Ac.Id.pp
        ; field "Architecture" emits Ac.Id.pp
        ; field "Command" cmd_line (hbox (list ~sep:sp string)) ])

  let pp_summary =
    Fmt.(
      concat ~sep:sp
        [ using style Ac.Id.pp
        ; using emits Ac.Id.pp
        ; using is_enabled Ac.Spec.pp_enabled_summary ])
end

module With_id = struct
  module W = Ac.Spec.Make_with_id (M)
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

include (Spec : module type of Spec with module With_id := Spec_basic.With_id)

include (M : module type of M with type t := M.t)
