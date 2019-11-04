(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Disable = struct
  module Location = struct
    type t = Machine | Spec
  end

  module Reason = struct
    type t = In_config | Filtered | Failed_test of Error.t
  end

  type t = {location: Location.t; reason: Reason.t}
  [@@deriving fields, make]

  let make_failed ~(location : Location.t) ~(error : Error.t) : t =
    make ~location ~reason:(Failed_test error)
end

type 'spec t =
  { disabled: ('spec Act_common.Spec.With_id.t * Disable.t) list
  ; enabled: 'spec Act_common.Spec.Set.t [@main] }
[@@deriving fields, make]

let get_with_fqid ?(id_type : string option)
    ?(default_machines : Act_common.Id.t list = []) (listing : 'spec t)
    ~(fqid : Act_common.Id.t) : 'spec Or_error.t =
  (* TODO(@MattWindsor91): on error, try looking up in the disabled specs,
     and emit a helpful error. *)
  Act_common.Spec.Set.get_with_fqid ?id_type ~prefixes:default_machines
    (enabled listing) ~fqid
