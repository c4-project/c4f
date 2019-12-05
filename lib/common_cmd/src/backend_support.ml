(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

let backends :
    (Ac.Id.t, (module Act_backend.Instance_types.Basic)) List.Assoc.t =
  [ (Ac.Id.of_string "herd", (module Act_backend_herd.Instance))
  ; (Ac.Id.of_string "litmus", (module Act_backend_litmus.Instance)) ]

let try_get_backend (style_id : Ac.Id.t) :
    (module Act_backend.Instance_types.Basic) Or_error.t =
  Ac.Id.try_find_assoc_with_suggestions backends style_id
    ~id_type:"backend style"

let resolve :
       Act_machine.Qualified.Backend.t
    -> (module Act_backend.Instance_types.S) Or_error.t =
  Act_machine.Qualified.Backend.lift_resolver
    ~f:(fun (sspec : Act_backend.Spec.With_id.t) ->
      sspec |> Act_common.Spec.With_id.spec |> Act_backend.Spec.style
      |> try_get_backend)

module Lookup = struct
  include Act_machine.Lookup.Backend (struct
    let test _spec = Or_error.return () (* for now *)
  end)

  (* TODO(@MattWindsor91): this seems exceptionally coupled, but it can't go
     into the lookup functor as it depends on Act_config. *)
  let lookup_in_cfg (fqid : Act_common.Id.t) ~(cfg : Act_config.Global.t) :
      Act_machine.Qualified.Backend.t Or_error.t =
    (* TODO(@MattWindsor91): there are probably ways we can declutter this *)
    let specs = Act_config.Global.machines cfg in
    let default_machines =
      Act_config.(Default.machines (Global.defaults cfg))
    in
    lookup_single ~default_machines specs ~fqid
end

let lookup_and_resolve_in_cfg (fqid : Act_common.Id.t)
    ~(cfg : Act_config.Global.t) :
    (module Act_backend.Instance_types.S) Or_error.t =
  Or_error.(fqid |> Lookup.lookup_in_cfg ~cfg >>= resolve)
