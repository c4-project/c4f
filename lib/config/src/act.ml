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
module Cq_spec = Act_machine.Qualified.Compiler
module Sq_spec = Act_machine.Qualified.Sim

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
  ; machines: M_spec.Set.t [@default Act_common.Spec.Set.empty]
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
      ; m_hook: M_spec.With_id.t hook
      ; c_hook: Cq_spec.t hook }
    [@@deriving fields, make]

    let add_disabled_compilers (ctx : t)
        ~(new_compilers : (Ac.Id.t, Error.t option) List.Assoc.t)
        ~(machine_id : Ac.Id.t) : t =
      let qualified_compilers =
        Tx.Alist.map_left new_compilers ~f:(fun compiler_id ->
            Ac.Id.(machine_id @. compiler_id))
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

    let get_machine_hook : M_spec.With_id.t hook t = peek Ctx.m_hook
  end

  module CP = Part_helpers (struct
    type t = Cq_spec.t

    let is_enabled x = C_spec.With_id.is_enabled (Cq_spec.c_spec x)

    let id x = C_spec.With_id.id (Cq_spec.c_spec x)
  end)

  module MP = Part_helpers (M_spec.With_id)
  module M_list = Tx.List.On_monad (M)
  module M_comp = M_spec.On_compiler_set.On_monad (M)

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

  let filter_compilers_in_machine_spec (spec : M_spec.t)
      ~(machine_id : Ac.Id.t) : M_spec.t M.t =
    (* TODO(@MattWindsor91): there's WAY too much with-id combining and
       separating here... *)
    let m_spec = M_spec.With_id.make ~spec ~id:machine_id in
    M_comp.map_m spec ~f:(filter_compilers ~m_spec)

  let filter_compilers_in_machine (m : M_spec.With_id.t) :
      M_spec.With_id.t M.t =
    let id = M_spec.With_id.id m in
    let spec = M_spec.With_id.spec m in
    M.Let_syntax.(
      let%map spec' =
        filter_compilers_in_machine_spec spec ~machine_id:id
      in
      M_spec.With_id.make ~id ~spec:spec')

  let filter_compilers_in_machines :
      M_spec.With_id.t list -> M_spec.With_id.t list M.t =
    M_list.map_m ~f:filter_compilers_in_machine

  let filter_machines (ms : M_spec.Set.t) : M_spec.Set.t M.t =
    M.Let_syntax.(
      let%bind hook = M.get_machine_hook in
      M_spec.Set.(
        let enabled, disabled =
          partition_map
            ~f:(part_chain_fst MP.part_enabled (MP.part_hook hook))
            ms
        in
        (* TODO(@MattWindsor91): test machines *)
        let%bind () = M.add_disabled_machines disabled in
        let%bind enabled' = filter_compilers_in_machines enabled in
        M.Monadic.return (of_list enabled')))

  let filter (m_hook : M_spec.With_id.t hook) (c_hook : Cq_spec.t hook)
      (raw_ms : M_spec.Set.t) : (Ctx.t * M_spec.Set.t) Or_error.t =
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

let fuzz = H.forward Global.fuzz

let defaults = H.forward Global.defaults

let default_machines : t -> Ac.Id.t list =
  Fn.compose Default.machines defaults

let compiler (cfg : t) ~(fqid : Ac.Id.t) : Cq_spec.t Or_error.t =
  Act_machine.Qualified.Lookup_compilers.lookup
    ~defaults:(default_machines cfg) (machines cfg) ~fqid

let sim (cfg : t) ~(fqid : Ac.Id.t) : Sq_spec.t Or_error.t =
  Act_machine.Qualified.Lookup_sims.lookup ~defaults:(default_machines cfg)
    (machines cfg) ~fqid

let all_compilers : t -> Cq_spec.t list =
  Fn.compose Act_machine.Qualified.Lookup_compilers.all machines

let all_sims : t -> Sq_spec.t list =
  Fn.compose Act_machine.Qualified.Lookup_sims.all machines
