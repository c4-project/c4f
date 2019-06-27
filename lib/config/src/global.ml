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
module Tx = Travesty_base_exts
module C_spec = Act_compiler.Spec
module M_spec = Act_machine.Spec

type t =
  { defaults: Default.t [@default Default.make ()] [@sexp.drop_if_default]
  ; fuzz: Fuzz.t option [@sexp.option]
  ; machines: M_spec.Set.t }
[@@deriving make, fields]

module Load : Plumbing.Loadable_types.S with type t = t = struct
  module File = struct
    let ssh (items : Ast.Ssh.t list) : Act_machine.Ssh.t Or_error.t =
      Or_error.Let_syntax.(
        let%map user =
          Au.My_list.find_one items ~item_name:"user" ~f:Ast.Ssh.as_user
        and host =
          Au.My_list.find_one items ~item_name:"host" ~f:Ast.Ssh.as_host
        and copy_dir =
          Au.My_list.find_one items ~item_name:"copy to"
            ~f:Ast.Ssh.as_copy_to
        in
        Act_machine.Ssh.make ~user ~host ~copy_dir ())

    let via = function
      | Ast.Via.Local ->
          Or_error.return Act_machine.Via.Local
      | Ssh items ->
          Or_error.(ssh items >>| Act_machine.Via.ssh)

    let simulator (items : Ast.Sim.t list) : Act_sim.Spec.t Or_error.t =
      Or_error.Let_syntax.(
        let%map cmd =
          Au.My_list.find_one_opt items ~item_name:"cmd" ~f:(function
            | Cmd c ->
                Some c
            | _ ->
                None)
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
        Act_sim.Spec.make ?cmd ?c_model ~asm_models ~style ())

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

    let try_get_named_simulator :
           Ast.Machine.t
        -> (Act_common.Id.t * Act_sim.Spec.t) option Or_error.t = function
      | Sim (n, s) ->
          Or_error.Let_syntax.(
            let%map s' = simulator s in
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
        ~(f : Ast.Machine.t -> (Act_common.Id.t * 'body) option Or_error.t)
        : 'body Ac.Spec.Set.t Or_error.t =
      Or_error.(
        items
        |> Tx.Or_error.combine_map ~f
        >>| List.filter_opt
        >>= Map.of_alist_or_error (module Act_common.Id)
        >>| Ac.Spec.Set.of_map)

    let try_get_named_simulators :
        Ast.Machine.t list -> Act_sim.Spec.t Ac.Spec.Set.t Or_error.t =
      try_get_named_specs ~f:try_get_named_simulator

    let try_get_named_compilers :
        Ast.Machine.t list -> Act_compiler.Spec.t Ac.Spec.Set.t Or_error.t =
      try_get_named_specs ~f:try_get_named_compiler

    let machine (items : Ast.Machine.t list) : M_spec.t Or_error.t =
      Or_error.Let_syntax.(
        let%bind enabled =
          Au.My_list.find_at_most_one items ~item_name:"enabled"
            ~f:(function Enabled b -> Some b | _ -> None)
            ~on_empty:(Or_error.return true)
        and sims = try_get_named_simulators items
        and compilers = try_get_named_compilers items
        and via_raw =
          Au.My_list.find_one items ~item_name:"via" ~f:(function
            | Via v ->
                Some v
            | _ ->
                None)
        in
        let%map via = via via_raw in
        M_spec.make ~sims ~compilers ~enabled ~via ())

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
        >>= M_spec.Set.of_list)

    let build_fuzz : Ast.t -> Fuzz.t option Or_error.t =
      Tx.Or_error.(
        Au.My_list.find_one_opt ~item_name:"fuzz" ~f:Ast.Top.as_fuzz
        >=> Tx.Option.With_errors.map_m ~f:Fuzz.of_ast)

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
