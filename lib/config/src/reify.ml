(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Fuzz = struct
  let reify_weight_alist :
      (C4f_common.Id.t, int) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, weight) -> Ast.Fuzz.Action (id, Some weight))

  let reify_weights : int Map.M(C4f_common.Id).t -> Ast.Fuzz.t list =
    Fn.compose reify_weight_alist Map.to_alist

  let reify_param_alist :
      (C4f_common.Id.t, int) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, value) ->
        Ast.Fuzz.Set (Ast.Fuzz.Setter.Param (id, value)) )

  let reify_params : int Map.M(C4f_common.Id).t -> Ast.Fuzz.t list =
    Fn.compose reify_param_alist Map.to_alist

  let reify_flag (f : C4f_fuzz.Flag.t) : Ast.Fuzz.Flag_value.t =
    match C4f_fuzz.Flag.to_exact_opt f with
    | Some b -> Ast.Fuzz.Flag_value.Exact b
    | None ->
        Ast.Fuzz.Flag_value.Ratio
          (C4f_fuzz.Flag.wins f, C4f_fuzz.Flag.losses f)

  let reify_flag_alist :
      (C4f_common.Id.t, C4f_fuzz.Flag.t) List.Assoc.t -> Ast.Fuzz.t list =
    List.map ~f:(fun (id, value) ->
        Ast.Fuzz.Set (Ast.Fuzz.Setter.Flag (id, reify_flag value)) )

  let reify_flags : C4f_fuzz.Flag.t Map.M(C4f_common.Id).t -> Ast.Fuzz.t list
      =
    Fn.compose reify_flag_alist Map.to_alist

  let reify (fuzz : C4f_fuzz_run.Config.t) : Ast.t =
    [ Fuzz
        (List.concat
           [ reify_weights (C4f_fuzz_run.Config.weights fuzz)
           ; reify_params (C4f_fuzz_run.Config.params fuzz)
           ; reify_flags (C4f_fuzz_run.Config.flags fuzz) ] ) ]
end

let reify (config : Global.t) : Ast.t = Fuzz.reify (Global.fuzz config)

let pp : Global.t Fmt.t = Fmt.using reify Ast.pp
