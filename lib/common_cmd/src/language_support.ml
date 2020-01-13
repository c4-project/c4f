(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common

(* Module shorthand *)
module C_spec = Act_compiler.Spec
module M_spec = Act_machine.Spec
module Mw_spec = M_spec.With_id
module Cq_spec = Act_machine.Qualified.Compiler
module C_types = Act_compiler.Instance_types

let style_modules : (Id.t, (module C_types.Basic)) List.Assoc.t =
  [(Id.of_string "gcc", (module Act_compiler_gcc.Instance))]

let try_get_style_module (style_id : Act_common.Id.t) :
    (module C_types.Basic) Or_error.t =
  Act_common.Id.try_find_assoc_with_suggestions style_modules style_id
    ~id_type:"compiler style"

let resolve :
       Act_machine.Qualified.Compiler.t
    -> (module Act_compiler.Instance_types.S) Or_error.t =
  Act_machine.Qualified.Compiler.lift_resolver
    ~f:(fun (cspec : C_spec.With_id.t) ->
      let style = C_spec.With_id.style cspec in
      try_get_style_module style)

module Lookup = struct
  include Act_machine.Lookup.Compiler (struct
    let test (spec : Act_machine.Qualified.Compiler.t) : unit Or_error.t =
      Or_error.Let_syntax.(
        let%bind (module Compiler) = resolve spec in
        Compiler.test ())
  end)

  (* TODO(@MattWindsor91): this seems exceptionally coupled, but it can't go
     into the lookup functor as it depends on Act_config. *)
  let lookup_in_cfg (fqid : Act_common.Id.t) ~(cfg : Act_config.Global.t) :
      Act_machine.Qualified.Compiler.t Or_error.t =
    (* TODO(@MattWindsor91): there are probably ways we can declutter this *)
    let specs = Act_config.Global.machines cfg in
    let default_machines =
      Act_config.(Default.machines (Global.defaults cfg))
    in
    lookup_single ~default_machines specs ~fqid
end

let lookup_and_resolve_in_cfg (fqid : Act_common.Id.t)
    ~(cfg : Act_config.Global.t) :
    (module Act_compiler.Instance_types.S) Or_error.t =
  Or_error.(fqid |> Lookup.lookup_in_cfg ~cfg >>= resolve)
