(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Au = Act_utils
module Tx = Travesty_core_kernel_exts
module C_spec = Act_compiler.Spec
module M_spec = Act_compiler.Machine_spec
module Cq_spec = M_spec.Qualified_compiler
module Sq_spec = M_spec.Qualified_sim

let part_chain_fst f g x = match f x with `Fst y -> g y | `Snd y -> `Snd y

(** Helpers for partitioning specs, parametrised on the spec type interface. *)
module Part_helpers (S : sig
  type t

  val is_enabled : t -> bool

  val id : t -> Ac.Id.t
end) =
struct
  (** [part_enabled x] is a partition_map function that sorts [x] into
      [`Fst] if they're enabled and [`Snd] if not. *)
  let part_enabled (x : S.t) =
    if S.is_enabled x then `Fst x else `Snd (S.id x, None)

  (** [part_hook hook x] is a partition_map function that runs [hook] on
      [x], and sorts the result into [`Fst] if it succeeded and [`Snd] if
      not. *)
  let part_hook (hook : S.t -> S.t option Or_error.t) (x : S.t) =
    match hook x with
    | Result.Ok (Some x') ->
        `Fst x'
    | Result.Ok None ->
        `Snd (S.id x, None)
    | Result.Error err ->
        `Snd (S.id x, Some err)
end

type t =
  { disabled_compilers: (Ac.Id.t, Error.t option) List.Assoc.t [@default []]
  ; disabled_machines: (Ac.Id.t, Error.t option) List.Assoc.t [@default []]
  ; machines: Act_compiler.Machine_spec.Set.t
        [@default Act_common.Spec.Set.empty]
  ; global: Global.t
  ; sanitiser_passes:
         default:Set.M(Act_sanitiser.Pass_group).t
      -> Set.M(Act_sanitiser.Pass_group).t }
[@@deriving fields, make]

type 't hook = 't -> 't option Or_error.t

module Filterer = struct
  (** ['t hook] is the type of testing hooks sent to [from_raw]. *)

  module Ctx = struct
    type t =
      { disabled_compilers: (Ac.Id.t, Error.t option) List.Assoc.t
            [@default []]
      ; disabled_machines: (Ac.Id.t, Error.t option) List.Assoc.t
            [@default []]
      ; m_hook: Act_compiler.Machine_spec.With_id.t hook
      ; c_hook: Cq_spec.t hook }
    [@@deriving fields, make]

    let add_disabled_compilers (ctx : t)
        ~(new_compilers : (Ac.Id.t, Error.t option) List.Assoc.t)
        ~(machine_id : Ac.Id.t) : t =
      let qualified_compilers =
        Tx.Alist.map_left new_compilers ~f:(fun compiler_id ->
            Ac.Id.(machine_id @. compiler_id) )
      in
      { ctx with
        disabled_compilers= ctx.disabled_compilers @ qualified_compilers }

    let add_disabled_machines (ctx : t)
        ~(new_machines : (Ac.Id.t, Error.t option) List.Assoc.t) : t =
      {ctx with disabled_machines= ctx.disabled_machines @ new_machines}
  end

  module M = struct
    include Travesty.State_transform.Make (struct
      type t = Ctx.t

      module Inner = Or_error
    end)

    let add_disabled_compilers
        (new_compilers : (Ac.Id.t, Error.t option) List.Assoc.t)
        ~(machine_id : Ac.Id.t) : unit t =
      modify (Ctx.add_disabled_compilers ~new_compilers ~machine_id)

    let add_disabled_machines
        (new_machines : (Ac.Id.t, Error.t option) List.Assoc.t) : unit t =
      modify (Ctx.add_disabled_machines ~new_machines)

    let get_compiler_hook : Cq_spec.t hook t = peek Ctx.c_hook

    let get_machine_hook : Act_compiler.Machine_spec.With_id.t hook t =
      peek Ctx.m_hook
  end

  module CP = Part_helpers (struct
    type t = Cq_spec.t

    let is_enabled x = C_spec.With_id.is_enabled (Cq_spec.c_spec x)

    let id x = C_spec.With_id.id (Cq_spec.c_spec x)
  end)

  module MP = Part_helpers (Act_compiler.Machine_spec.With_id)
  module M_list = Tx.List.On_monad (M)
  module M_comp = Act_compiler.Machine_spec.On_compiler_set.On_monad (M)

  let part_compilers hook (m_spec : M_spec.With_id.t)
      (c_spec : C_spec.With_id.t) =
    let q_spec = Cq_spec.make ~m_spec ~c_spec in
    (part_chain_fst CP.part_enabled (CP.part_hook hook)) q_spec

  let filter_compilers (cs : C_spec.Set.t) ~(m_spec : M_spec.With_id.t) :
      C_spec.Set.t M.t =
    M.Let_syntax.(
      let%bind hook = M.get_compiler_hook in
      C_spec.Set.(
        let enabled, disabled =
          partition_map ~f:(part_compilers hook m_spec) cs
        in
        (* TODO(@MattWindsor91): move compiler testing here. *)
        let machine_id = M_spec.With_id.id m_spec in
        let%bind () = M.add_disabled_compilers ~machine_id disabled in
        M.Monadic.return
          (C_spec.Set.of_list (List.map ~f:Cq_spec.c_spec enabled))))

  let filter_compilers_in_machine_spec (spec : Act_compiler.Machine_spec.t)
      ~(machine_id : Ac.Id.t) : Act_compiler.Machine_spec.t M.t =
    (* TODO(@MattWindsor91): there's WAY too much with-id combining and
       separating here... *)
    let m_spec = M_spec.With_id.make ~spec ~id:machine_id in
    M_comp.map_m spec ~f:(filter_compilers ~m_spec)

  let filter_compilers_in_machine (m : Act_compiler.Machine_spec.With_id.t)
      : Act_compiler.Machine_spec.With_id.t M.t =
    let id = Act_compiler.Machine_spec.With_id.id m in
    let spec = Act_compiler.Machine_spec.With_id.spec m in
    M.Let_syntax.(
      let%map spec' =
        filter_compilers_in_machine_spec spec ~machine_id:id
      in
      Act_compiler.Machine_spec.With_id.make ~id ~spec:spec')

  let filter_compilers_in_machines :
         Act_compiler.Machine_spec.With_id.t list
      -> Act_compiler.Machine_spec.With_id.t list M.t =
    M_list.map_m ~f:filter_compilers_in_machine

  let filter_machines (ms : Act_compiler.Machine_spec.Set.t) :
      Act_compiler.Machine_spec.Set.t M.t =
    M.Let_syntax.(
      let%bind hook = M.get_machine_hook in
      Act_compiler.Machine_spec.Set.(
        let enabled, disabled =
          partition_map
            ~f:(part_chain_fst MP.part_enabled (MP.part_hook hook))
            ms
        in
        (* TODO(@MattWindsor91): test machines *)
        let%bind () = M.add_disabled_machines disabled in
        let%bind enabled' = filter_compilers_in_machines enabled in
        M.Monadic.return (of_list enabled')))

  let filter (m_hook : Act_compiler.Machine_spec.With_id.t hook)
      (c_hook : Cq_spec.t hook) (raw_ms : Act_compiler.Machine_spec.Set.t) :
      (Ctx.t * Act_compiler.Machine_spec.Set.t) Or_error.t =
    M.run' (filter_machines raw_ms) (Ctx.make ~m_hook ~c_hook ())
end

let of_global ?(chook = Fn.compose Result.return Option.some)
    ?(mhook = Fn.compose Result.return Option.some)
    ?(phook = fun ~default -> default) (global : Global.t) : t Or_error.t =
  let raw_ms = Global.machines global in
  Or_error.Let_syntax.(
    let%map filter_ctx, machines = Filterer.filter mhook chook raw_ms in
    let disabled_machines = Filterer.Ctx.disabled_machines filter_ctx in
    let disabled_compilers = Filterer.Ctx.disabled_compilers filter_ctx in
    make ~global ~machines ~disabled_compilers ~disabled_machines
      ~sanitiser_passes:phook ())

module H = Act_utils.Inherit.Helpers (struct
  type nonrec t = t

  type c = Global.t

  let component = global
end)

let cpp = H.forward Global.cpp

let fuzz = H.forward Global.fuzz

let defaults = H.forward Global.defaults

let machine_of_fqid (cfg : t) ~(fqid : Ac.Id.t) :
    M_spec.With_id.t Or_error.t =
  cfg |> machines |> Act_compiler.Machine_spec.Set.get_using_fqid ~fqid

let compiler (cfg : t) ~(fqid : Ac.Id.t) : Cq_spec.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind m_spec = machine_of_fqid cfg ~fqid in
    let%bind compiler_id =
      Ac.Id.drop_prefix fqid ~prefix:(M_spec.With_id.id m_spec)
    in
    let compilers = M_spec.With_id.compilers m_spec in
    let%map c_spec = C_spec.Set.get compilers compiler_id in
    Cq_spec.make ~c_spec ~m_spec)

let sim (cfg : t) ~(fqid : Ac.Id.t) : Sq_spec.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind m_spec = machine_of_fqid cfg ~fqid in
    let%bind sim_id =
      Ac.Id.drop_prefix fqid ~prefix:(M_spec.With_id.id m_spec)
    in
    let sims = M_spec.With_id.sims m_spec in
    let%map s_spec = Act_sim.Spec.Set.get sims sim_id in
    Sq_spec.make ~s_spec ~m_spec)

let all (cfg : t) ~(f : M_spec.With_id.t -> 'a list) : 'a list =
  cfg |> machines |> M_spec.Set.map ~f |> List.concat

let qualified_compilers_for_machine (m_spec : M_spec.With_id.t) :
    Cq_spec.t list =
  m_spec |> M_spec.With_id.compilers
  |> C_spec.Set.map ~f:(fun c_spec -> Cq_spec.make ~m_spec ~c_spec)

let qualified_sims_for_machine (m_spec : M_spec.With_id.t) : Sq_spec.t list
    =
  m_spec |> M_spec.With_id.sims
  |> Act_sim.Spec.Set.map ~f:(fun s_spec -> Sq_spec.make ~m_spec ~s_spec)

let all_compilers : t -> Cq_spec.t list =
  all ~f:qualified_compilers_for_machine

let all_sims : t -> M_spec.Qualified_sim.t list =
  all ~f:qualified_sims_for_machine
