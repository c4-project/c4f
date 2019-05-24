(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Ac = Act_common
module Au = Act_utils
module Tx = Travesty_core_kernel_exts
include Act_intf

module Raw = struct
  module C = Compiler.Cfg_spec

  type t =
    { cpp: Cpp.t option [@sexp.option]
    ; herd: Herd.t option [@sexp.option]
    ; fuzz: Fuzz.t option [@sexp.option]
    ; compilers: C.Set.t
    ; machines: Machine.Spec.Set.t }
  [@@deriving sexp, make, fields]

  let sanitiser_passes _ ~default = default

  module Raw_load = struct
    (** Reading in config from an AST. *)
    module File = struct
      let ssh (items : Ast.Ssh.t list) : Machine.Ssh.t Or_error.t =
        Or_error.Let_syntax.(
          let%map user =
            Au.My_list.find_one items ~item_name:"user" ~f:(function
              | User u ->
                  Some u
              | _ ->
                  None )
          and host =
            Au.My_list.find_one items ~item_name:"host" ~f:(function
              | Host h ->
                  Some h
              | _ ->
                  None )
          and copy_dir =
            Au.My_list.find_one items ~item_name:"copy to" ~f:(function
              | Copy_to c ->
                  Some c
              | _ ->
                  None )
          in
          Machine.Ssh.make ~user ~host ~copy_dir ())

      let via = function
        | Ast.Via.Local ->
            Or_error.return Machine.Via.Local
        | Ssh items ->
            Or_error.(ssh items >>| Machine.Via.ssh)

      let litmus (items : Ast.Litmus.t list) : Litmus_tool.t Or_error.t =
        Or_error.Let_syntax.(
          let%map cmd =
            Au.My_list.find_one_opt items ~item_name:"cmd" ~f:(function
                | Cmd c -> Some c (* | _ -> None *) )
          in
          Litmus_tool.make ?cmd ())

      let machine (items : Ast.Machine.t list) : Machine.Spec.t Or_error.t =
        Or_error.Let_syntax.(
          let%bind enabled =
            Au.My_list.find_at_most_one items ~item_name:"enabled"
              ~f:(function Enabled b -> Some b | _ -> None)
              ~on_empty:(Or_error.return true)
          and litmus_raw =
            Au.My_list.find_one_opt items ~item_name:"litmus" ~f:(function
              | Litmus h ->
                  Some h
              | _ ->
                  None )
          and via_raw =
            Au.My_list.find_one items ~item_name:"via" ~f:(function
              | Via v ->
                  Some v
              | _ ->
                  None )
          in
          let%map litmus = Tx.Option.With_errors.map_m ~f:litmus litmus_raw
          and via = via via_raw in
          Machine.Spec.make ?litmus ~enabled ~via ())

      let compiler (items : Ast.Compiler.t list) : C.t Or_error.t =
        Or_error.Let_syntax.(
          let%map enabled =
            Au.My_list.find_at_most_one items ~item_name:"enabled"
              ~f:(function Enabled b -> Some b | _ -> None)
              ~on_empty:(Or_error.return true)
          and style =
            Au.My_list.find_one items ~item_name:"style" ~f:(function
              | Style s ->
                  Some (Ac.Id.to_string s)
              | _ ->
                  None )
          and emits =
            Au.My_list.find_one items ~item_name:"emits" ~f:(function
              | Emits e ->
                  Some e
              | _ ->
                  None )
          and cmd =
            Au.My_list.find_one items ~item_name:"cmd" ~f:(function
              | Cmd c ->
                  Some c
              | _ ->
                  None )
          and argv =
            Au.My_list.find_at_most_one items ~item_name:"argv"
              ~f:(function Argv v -> Some v | _ -> None)
              ~on_empty:(return [])
          and herd =
            Au.My_list.find_at_most_one items ~item_name:"herd"
              ~f:(function Herd h -> Some h | _ -> None)
              ~on_empty:(return true)
          and machine =
            Au.My_list.find_at_most_one items ~item_name:"copy to"
              ~f:(function Machine m -> Some m | _ -> None)
              ~on_empty:(return Machine.Id.default)
          in
          Compiler.Cfg_spec.make ~enabled ~style ~emits ~cmd ~argv ~herd
            ~machine ())

      let cpp (items : Ast.Cpp.t list) =
        Or_error.Let_syntax.(
          let%map cmd =
            Au.My_list.find_one_opt items ~item_name:"cmd" ~f:(function
              | Cmd c ->
                  Some c
              | _ ->
                  None )
          and argv =
            Au.My_list.find_one_opt items ~item_name:"argv" ~f:(function
              | Argv v ->
                  Some v
              | _ ->
                  None )
          and enabled =
            Au.My_list.find_at_most_one items ~item_name:"enabled"
              ~f:(function Enabled b -> Some b | _ -> None)
              ~on_empty:(return true)
          in
          Cpp.make ~enabled ?cmd ?argv ())

      let herd (items : Ast.Herd.t list) =
        Or_error.Let_syntax.(
          let%map cmd =
            Au.My_list.find_one_opt items ~item_name:"cmd" ~f:(function
              | Cmd c ->
                  Some c
              | _ ->
                  None )
          and c_model =
            Au.My_list.find_one_opt items ~item_name:"c_model" ~f:(function
              | C_model c ->
                  Some c
              | _ ->
                  None )
          in
          let asm_models =
            List.filter_map items ~f:(function
              | Asm_model (k, v) ->
                  Some (k, v)
              | _ ->
                  None )
          in
          Herd.make ?c_model ~asm_models ?cmd ())

      let build_cpp (items : Ast.t) =
        Or_error.Let_syntax.(
          let cpp_ast_result =
            Au.My_list.find_one_opt items ~item_name:"cpp" ~f:Ast.Top.as_cpp
          in
          match%bind cpp_ast_result with
          | Some cpp_ast ->
              cpp cpp_ast >>| Option.some
          | None ->
              return None)

      let build_herd (items : Ast.t) =
        Or_error.Let_syntax.(
          let herd_ast_result =
            Au.My_list.find_one_opt items ~item_name:"herd"
              ~f:Ast.Top.as_herd
          in
          match%bind herd_ast_result with
          | Some herd_ast ->
              herd herd_ast >>| Option.some
          | None ->
              return None)

      let build_machines (items : Ast.t) =
        Or_error.Let_syntax.(
          items
          |> List.filter_map ~f:Ast.Top.as_machine
          |> Tx.List.With_errors.map_m ~f:(fun (id, spec_ast) ->
                 let%map spec = machine spec_ast in
                 Machine.Spec.With_id.make ~id ~spec )
          >>= Machine.Spec.Set.of_list)

      let build_compilers (items : Ast.t) =
        Or_error.Let_syntax.(
          items
          |> List.filter_map ~f:Ast.Top.as_compiler
          |> Tx.List.With_errors.map_m ~f:(fun (id, spec_ast) ->
                 let%map spec = compiler spec_ast in
                 Compiler.Cfg_spec.With_id.make ~id ~spec )
          >>= Compiler.Cfg_spec.Set.of_list)

      let build_fuzz (items : Ast.t) : Fuzz.t option Or_error.t =
        Or_error.Let_syntax.(
          items
          |> Au.My_list.find_one_opt ~item_name:"fuzz" ~f:Ast.Top.as_fuzz
          >>= Tx.Option.With_errors.map_m ~f:Fuzz.of_ast)

      let main (items : Ast.t) : t Or_error.t =
        Or_error.Let_syntax.(
          let%map cpp = build_cpp items
          and herd = build_herd items
          and fuzz = build_fuzz items
          and machines = build_machines items
          and compilers = build_compilers items in
          make ?cpp ?herd ?fuzz ~machines ~compilers ())
    end

    include Au.Loadable.Make_chain
              (Frontend)
              (struct
                type dst = t

                let f = File.main
              end)
  end

  include (Raw_load : Au.Loadable_intf.S with type t := t)
end

let part_chain_fst f g x = match f x with `Fst y -> g y | `Snd y -> `Snd y

(** Helpers for partitioning specs, parametrised on the spec type interface. *)
module Part_helpers (S : Ac.Spec.S) = struct
  (** [part_enabled x] is a partition_map function that sorts [x] into
      [`Fst] if they're enabled and [`Snd] if not. *)
  let part_enabled (x : S.With_id.t) =
    if S.With_id.is_enabled x then `Fst x else `Snd (S.With_id.id x, None)

  (** [part_hook hook x] is a partition_map function that runs [hook] on
      [x], and sorts the result into [`Fst] if it succeeded and [`Snd] if
      not. *)
  let part_hook (hook : S.With_id.t -> S.With_id.t option Or_error.t)
      (x : S.With_id.t) =
    match hook x with
    | Result.Ok (Some x') ->
        `Fst x'
    | Result.Ok None ->
        `Snd (S.With_id.id x, None)
    | Result.Error err ->
        `Snd (S.With_id.id x, Some err)
end

module M = struct
  module C = Compiler.Spec

  type t =
    { compilers: C.Set.t
    ; machines: Machine.Spec.Set.t
    ; cpp: Cpp.t option [@sexp.option]
    ; fuzz: Fuzz.t option [@sexp.option]
    ; herd: Herd.t option [@sexp.option]
    ; sanitiser_passes:
           default:Set.M(Act_sanitiser.Pass_group).t
        -> Set.M(Act_sanitiser.Pass_group).t
    ; disabled_compilers: (Ac.Id.t, Error.t option) List.Assoc.t
    ; disabled_machines: (Ac.Id.t, Error.t option) List.Assoc.t }
  [@@deriving sexp, fields]

  module RP = Part_helpers (Raw.C)
  module CP = Part_helpers (C)
  module MP = Part_helpers (Machine.Spec)

  (** ['t hook] is the type of testing hooks sent to [from_raw]. *)
  type 't hook = 't -> 't option Or_error.t

  let machines_from_raw (hook : Machine.Spec.With_id.t hook)
      (ms : Machine.Spec.Set.t) :
      (Machine.Spec.Set.t * (Ac.Id.t, Error.t option) List.Assoc.t)
      Or_error.t =
    let open Or_error.Let_syntax in
    Machine.Spec.Set.(
      let enabled, disabled =
        partition_map
          ~f:(part_chain_fst MP.part_enabled (MP.part_hook hook))
          ms
      in
      (* TODO(@MattWindsor91): test machines *)
      let%map enabled' = Machine.Spec.Set.of_list enabled in
      (enabled', disabled))

  let build_compiler (rawc : Raw.C.With_id.t)
      (mach : Machine.Spec.With_id.t) : C.t =
    Raw.C.With_id.(
      C.make ~enabled:(is_enabled rawc) ~style:(style rawc)
        ~emits:(emits rawc) ~cmd:(cmd rawc) ~argv:(argv rawc)
        ~herd:(herd rawc) ~machine:mach ())

  let find_machine enabled disabled mach =
    Or_error.(
      match Machine.Spec.Set.get enabled mach with
      | Ok m ->
          return (`Fst m)
      | _ -> (
        match List.Assoc.find ~equal:Ac.Id.equal disabled mach with
        | Some e ->
            return (`Snd (mach, e))
        | None ->
            error_s [%message "Machine doesn't exist" ~id:(mach : Ac.Id.t)]
        ))

  let part_resolve enabled disabled c =
    let machine_id = Raw.C.With_id.machine c in
    match find_machine enabled disabled machine_id with
    (* Machine enabled *)
    | Result.Ok (`Fst mach) ->
        `Fst
          (C.With_id.make ~id:(Raw.C.With_id.id c)
             ~spec:(build_compiler c mach))
    (* Machine disabled, possibly because of error *)
    | Result.Ok (`Snd (_, err)) ->
        `Snd
          ( Raw.C.With_id.id c
          , Option.map
              ~f:(Error.tag ~tag:"Machine was disabled because:")
              err )
    (* Error actually finding the machine *)
    | Result.Error err ->
        `Snd
          ( Raw.C.With_id.id c
          , Some (Error.tag ~tag:"Error finding machine:" err) )

  let herd_or_default (conf : t) : Herd.t =
    conf |> herd |> Tx.Option.value_f ~default_f:Herd.default

  let compilers_from_raw (ms : Machine.Spec.Set.t)
      (ms_disabled : (Ac.Id.t, Error.t option) List.Assoc.t)
      (hook : C.With_id.t hook) (cs : Raw.C.Set.t) :
      (C.Set.t * (Ac.Id.t, Error.t option) List.Assoc.t) Or_error.t =
    let open Or_error.Let_syntax in
    Raw.C.Set.(
      let enabled, disabled =
        partition_map
          ~f:
            (part_chain_fst
               (* First, remove disabled compilers... *)
               RP.part_enabled
               (* ...then, resolve machine IDs and remove compilers with no
                  enabled machine... *)
               (part_chain_fst
                  (part_resolve ms ms_disabled)
                  (* ...then, run the given testing/filtering hook. *)
                  (CP.part_hook hook)))
          cs
      in
      (* TODO(@MattWindsor91): move compiler testing here. *)
      let%map enabled' = C.Set.of_list enabled in
      (enabled', disabled))

  let from_raw ?(chook = Fn.compose Result.return Option.some)
      ?(mhook = Fn.compose Result.return Option.some)
      ?(phook = fun ~default -> default) (c : Raw.t) : t Or_error.t =
    let raw_ms = Raw.machines c in
    let raw_cs = Raw.compilers c in
    Or_error.Let_syntax.(
      let%bind machines, disabled_machines =
        machines_from_raw mhook raw_ms
      in
      let%map compilers, disabled_compilers =
        compilers_from_raw machines disabled_machines chook raw_cs
      in
      { compilers
      ; machines
      ; disabled_compilers
      ; disabled_machines
      ; sanitiser_passes= phook
      ; cpp= Raw.cpp c
      ; fuzz= Raw.fuzz c
      ; herd= Raw.herd c })
end

include M
