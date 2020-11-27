(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

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

  let reify (fuzz : Act_fuzz_run.Config.t) : Ast.t =
    [ Fuzz
        (List.concat
           [ reify_weights (Act_fuzz_run.Config.weights fuzz)
           ; reify_params (Act_fuzz_run.Config.params fuzz)
           ; reify_flags (Act_fuzz_run.Config.flags fuzz) ]) ]
end

let reify (config : Global.t) : Ast.t = Fuzz.reify (Global.fuzz config)

let pp : Global.t Fmt.t = Fmt.using reify Ast.pp
