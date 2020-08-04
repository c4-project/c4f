(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Defaults = struct
  let reify_try (category : Ast.Default.Category.t) :
      Act_common.Id.t list -> Ast.Default.t list =
    List.map ~f:(fun id -> Ast.Default.Try (category, id))

  let reify (defaults : Default.t) : Ast.t =
    [ Default
        (List.concat
           [ reify_try Arch (Default.arches defaults)
           ; reify_try Machine (Default.machines defaults)
           ; reify_try Backend (Default.backends defaults) ]) ]
end

module Fuzz = struct
  let reify_weight_alist :
      (Act_common.Id.t, int) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, weight) -> Ast.Fuzz.Action (id, Some weight))

  let reify_weights : int Map.M(Act_common.Id).t -> Ast.Fuzz.t list =
    Fn.compose reify_weight_alist Map.to_alist

  let reify_param_alist :
      (Act_common.Id.t, int) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, value) ->
        Ast.Fuzz.Set (Ast.Fuzz.Setter.Param (id, value)))

  let reify_params : int Map.M(Act_common.Id).t -> Ast.Fuzz.t list =
    Fn.compose reify_param_alist Map.to_alist

  let reify_flag (f : Act_fuzz.Flag.t) : Ast.Fuzz.Flag_value.t =
    match Act_fuzz.Flag.to_exact_opt f with
    | Some b ->
        Ast.Fuzz.Flag_value.Exact b
    | None ->
        Ast.Fuzz.Flag_value.Ratio
          (Act_fuzz.Flag.wins f, Act_fuzz.Flag.losses f)

  let reify_flag_alist :
      (Act_common.Id.t, Act_fuzz.Flag.t) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, value) ->
        Ast.Fuzz.Set (Ast.Fuzz.Setter.Flag (id, reify_flag value)))

  let reify_flags : Act_fuzz.Flag.t Map.M(Act_common.Id).t -> Ast.Fuzz.t list
      =
    Fn.compose reify_flag_alist Map.to_alist

  let reify (fuzz : Act_fuzz.Config.t) : Ast.t =
    [ Fuzz
        (List.concat
           [ reify_weights (Act_fuzz.Config.weights fuzz)
           ; reify_params (Act_fuzz.Config.params fuzz)
           ; reify_flags (Act_fuzz.Config.flags fuzz) ]) ]
end

module Machines = struct
  let reify_spec_set (type spec result)
      ~(f : Act_common.Id.t -> spec -> result)
      (specs : spec Act_common.Spec.Set.t) : result list =
    specs |> Act_common.Spec.Set.to_list
    |> List.map ~f:(fun wid ->
           Act_common.Spec.With_id.(f (id wid) (spec wid)))

  let reify_backend_spec (spec : Act_backend.Spec.t) : Ast.Backend.t list =
    List.concat
      Act_backend.Spec.
        [ [Ast.Backend.Style (style spec); Cmd (cmd spec)]
        ; (let a = argv spec in
           if List.is_empty a then [] else [Ast.Backend.Argv a])
        ; List.map
            ~f:(fun str -> Ast.Backend.C_model str)
            (Option.to_list (c_model spec))
        ; List.map
            ~f:(fun (id, str) -> Ast.Backend.Asm_model (id, str))
            (asm_models spec) ]

  let reify_backends :
      Act_backend.Spec.t Act_common.Spec.Set.t -> Ast.Machine.t list =
    reify_spec_set ~f:(fun id spec ->
        Ast.Machine.Backend (id, reify_backend_spec spec))

  let reify_ssh (cfg : Plumbing.Ssh_runner.Config.t) : Ast.Ssh.t list =
    List.filter_opt
      Plumbing.Ssh_runner.Config.
        [ Option.map ~f:(fun x -> Ast.Ssh.User x) (user cfg)
        ; Some (Host (host cfg))
        ; Some (Copy_to (copy_dir cfg)) ]

  let reify_via : Act_machine.Via.t -> Ast.Via.t = function
    | Local ->
        Local
    | Ssh cfg ->
        Ssh (reify_ssh cfg)

  let reify_standalone_items (spec : Act_machine.Spec.t) : Ast.Machine.t list
      =
    Act_machine.Spec.[Enabled (is_enabled spec); Via (reify_via (via spec))]

  let reify_spec (spec : Act_machine.Spec.t) : Ast.Machine.t list =
    List.concat
      [ reify_standalone_items spec
      ; reify_backends (Act_machine.Spec.backends spec) ]

  let reify_spec_with_id (wid : Act_machine.Spec.t Act_common.Spec.With_id.t)
      : Ast.Top.t =
    Ast.Top.Machine
      ( Act_common.Spec.With_id.id wid
      , reify_spec (Act_common.Spec.With_id.spec wid) )

  let reify (machines : Act_machine.Spec.t Act_common.Spec.Set.t) : Ast.t =
    machines |> Act_common.Spec.Set.to_list |> List.map ~f:reify_spec_with_id

  let pp : Act_machine.Spec.t Act_common.Spec.Set.t Fmt.t =
    Fmt.using reify Ast.pp
end

let reify (config : Global.t) : Ast.t =
  List.concat
    [ Defaults.reify (Global.defaults config)
    ; Fuzz.reify (Global.fuzz config)
    ; Machines.reify (Global.machines config) ]

let pp : Global.t Fmt.t = Fmt.using reify Ast.pp
