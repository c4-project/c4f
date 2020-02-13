(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* We can't use 'open struct' here, because the sedlex PPX only supports the
   4.07 AST. Aargh! *)

module Ac = Act_common
module Au = Act_utils
module Tx = Travesty_base_exts
module C_spec = Act_compiler.Spec
module M_spec = Act_machine.Spec

type t =
  { defaults: Default.t [@default Default.make ()] [@sexp.drop_if_default]
  ; fuzz: Act_fuzz.Config.t [@sexp.option] [@default Act_fuzz.Config.make ()]
  ; machines: M_spec.Set.t }
[@@deriving make, fields]

module Load : Plumbing.Loadable_types.S with type t = t = struct
  module File = struct
    let ssh_remote (items : Ast.Ssh.t list) : Plumbing.Ssh.t Or_error.t =
      Or_error.Let_syntax.(
        let%map user =
          Au.My_list.find_one items ~item_name:"user" ~f:Ast.Ssh.as_user
        and host =
          Au.My_list.find_one items ~item_name:"host" ~f:Ast.Ssh.as_host
        in
        Plumbing.Ssh.make ~user ~host ())

    let ssh (items : Ast.Ssh.t list) :
        Plumbing.Ssh_runner.Config.t Or_error.t =
      Or_error.Let_syntax.(
        let%map remote = ssh_remote items
        and copy_dir =
          Au.My_list.find_one items ~item_name:"copy to"
            ~f:Ast.Ssh.as_copy_to
        in
        Plumbing.Ssh_runner.Config.make ~remote ~copy_dir)

    let via = function
      | Ast.Via.Local ->
          Or_error.return Act_machine.Via.Local
      | Ssh items ->
          Or_error.(ssh items >>| Act_machine.Via.ssh)

    let backends (items : Ast.Backend.t list) : Act_backend.Spec.t Or_error.t
        =
      Or_error.Let_syntax.(
        let%map cmd =
          Au.My_list.find_one items ~item_name:"cmd" ~f:(function
            | Cmd c ->
                Some c
            | _ ->
                None)
        and argv =
          Au.My_list.find_at_most_one items ~item_name:"argv"
            ~f:(function Argv v -> Some v | _ -> None)
            ~on_empty:(return [])
        and c_model =
          Au.My_list.find_one_opt items ~item_name:"c_model" ~f:(function
            | C_model c ->
                Some c
            | _ ->
                None)
        and style =
          Au.My_list.find_one items ~item_name:"style" ~f:(function
            | Style s ->
                Some s
            | _ ->
                None)
        in
        let asm_models =
          List.filter_map items ~f:(function
            | Asm_model (k, v) ->
                Some (k, v)
            | _ ->
                None)
        in
        Act_backend.Spec.make ~cmd ?c_model ~argv ~asm_models ~style ())

    let compiler (items : Ast.Compiler.t list) : C_spec.t Or_error.t =
      Or_error.Let_syntax.(
        let%map enabled =
          Au.My_list.find_at_most_one items ~item_name:"enabled"
            ~f:(function Enabled b -> Some b | _ -> None)
            ~on_empty:(Or_error.return true)
        and style =
          Au.My_list.find_one items ~item_name:"style" ~f:(function
            | Style s ->
                Some s
            | _ ->
                None)
        and emits =
          Au.My_list.find_one items ~item_name:"emits" ~f:(function
            | Emits e ->
                Some e
            | _ ->
                None)
        and cmd =
          Au.My_list.find_one items ~item_name:"cmd" ~f:(function
            | Cmd c ->
                Some c
            | _ ->
                None)
        and argv =
          Au.My_list.find_at_most_one items ~item_name:"argv"
            ~f:(function Argv v -> Some v | _ -> None)
            ~on_empty:(return [])
        in
        Act_compiler.Spec.make ~enabled ~style ~emits ~cmd ~argv ())

    let try_get_named_backends :
           Ast.Machine.t
        -> (Act_common.Id.t * Act_backend.Spec.t) option Or_error.t =
      function
      | Backend (n, s) ->
          Or_error.Let_syntax.(
            let%map s' = backends s in
            Some (n, s'))
      | _ ->
          Or_error.return None

    let try_get_named_compiler :
           Ast.Machine.t
        -> (Act_common.Id.t * Act_compiler.Spec.t) option Or_error.t =
      function
      | Compiler (n, s) ->
          Or_error.Let_syntax.(
            let%map s' = compiler s in
            Some (n, s'))
      | _ ->
          Or_error.return None

    let try_get_named_specs (items : Ast.Machine.t list)
        ~(f : Ast.Machine.t -> (Act_common.Id.t * 'body) option Or_error.t) :
        'body Ac.Spec.Set.t Or_error.t =
      Or_error.(
        items
        |> Tx.Or_error.combine_map ~f
        >>| List.filter_opt
        >>= Map.of_alist_or_error (module Act_common.Id)
        >>| Ac.Spec.Set.of_map)

    let try_get_named_backendss :
        Ast.Machine.t list -> Act_backend.Spec.t Ac.Spec.Set.t Or_error.t =
      try_get_named_specs ~f:try_get_named_backends

    let try_get_named_compilers :
        Ast.Machine.t list -> Act_compiler.Spec.t Ac.Spec.Set.t Or_error.t =
      try_get_named_specs ~f:try_get_named_compiler

    let machine (items : Ast.Machine.t list) : M_spec.t Or_error.t =
      Or_error.Let_syntax.(
        let%bind enabled =
          Au.My_list.find_at_most_one items ~item_name:"enabled"
            ~f:(function Enabled b -> Some b | _ -> None)
            ~on_empty:(Or_error.return true)
        and backends = try_get_named_backendss items
        and compilers = try_get_named_compilers items
        and via_raw =
          Au.My_list.find_one items ~item_name:"via" ~f:(function
            | Via v ->
                Some v
            | _ ->
                None)
        in
        let%map via = via via_raw in
        M_spec.make ~backends ~compilers ~enabled ~via ())

    let machine_with_id (id : Ac.Id.t) (spec_ast : Ast.Machine.t list) :
        M_spec.With_id.t Or_error.t =
      Or_error.Let_syntax.(
        let%map spec = machine spec_ast in
        M_spec.With_id.make ~id ~spec)

    let build_machines (items : Ast.t) =
      Or_error.(
        items
        |> List.filter_map ~f:Ast.Top.as_machine
        |> Tx.List.With_errors.map_m ~f:(fun (i, s) -> machine_with_id i s)
        >>= Act_common.Spec.Set.of_list)

    let to_weight_opt : Ast.Fuzz.t -> (Act_common.Id.t * int) option =
      function
      | Ast.Fuzz.Action (a, w) ->
          Some (a, Option.value w ~default:1)
      | Set _ ->
          None

    let to_param_opt : Ast.Fuzz.t -> (Act_common.Id.t * int) option =
      function
      | Ast.Fuzz.Set (Param (k, v)) ->
          Some (k, v)
      | Set _ | Action _ ->
          None

    let interpret_flag : Ast.Fuzz.Flag_value.t -> Act_fuzz.Flag.t Or_error.t
        = function
      | Exact b ->
          Or_error.return (Act_fuzz.Flag.exact b)
      | Ratio (wins, losses) ->
          Act_fuzz.Flag.try_make ~wins ~losses

    let to_flag_opt :
        Ast.Fuzz.t -> (Act_common.Id.t * Act_fuzz.Flag.t) Or_error.t option =
      function
      | Ast.Fuzz.Set (Flag (k, f)) ->
          Some (Or_error.map ~f:(fun v -> (k, v)) (interpret_flag f))
      | Set _ | Action _ ->
          None

    let fuzz_of_ast (ast : Ast.Fuzz.t list) : Act_fuzz.Config.t Or_error.t =
      let weight_alist = List.filter_map ast ~f:to_weight_opt in
      let param_alist = List.filter_map ast ~f:to_param_opt in
      Or_error.Let_syntax.(
        let%bind flag_alist =
          Or_error.combine_errors (List.filter_map ast ~f:to_flag_opt)
        in
        let%bind weights =
          Map.of_alist_or_error (module Act_common.Id) weight_alist
        in
        let%bind params =
          Map.of_alist_or_error (module Act_common.Id) param_alist
        in
        let%map flags =
          Map.of_alist_or_error (module Act_common.Id) flag_alist
        in
        Act_fuzz.Config.make ~weights ~flags ~params ())

    let build_fuzz : Ast.t -> Act_fuzz.Config.t option Or_error.t =
      Tx.Or_error.(
        Au.My_list.find_one_opt ~item_name:"fuzz" ~f:Ast.Top.as_fuzz
        >=> Tx.Option.With_errors.map_m ~f:fuzz_of_ast)

    let build_defaults : Ast.t -> Default.t Or_error.t =
      Tx.Or_error.(
        Au.My_list.find_one_opt ~item_name:"default" ~f:Ast.Top.as_default
        >=> Tx.Option.With_errors.map_m ~f:Default.of_ast
        >=> fun xo ->
        Or_error.return (Option.value xo ~default:(Default.make ())))

    let f (items : Ast.t) : t Or_error.t =
      Or_error.Let_syntax.(
        let%map fuzz = build_fuzz items
        and machines = build_machines items
        and defaults = build_defaults items in
        make ?fuzz ~machines ~defaults ())

    type dst = t
  end

  include Plumbing.Loadable.Make_chain (Frontend) (File)
end
