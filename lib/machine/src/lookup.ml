(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Qb = Qualified.Backend

module Machine = struct
  let partition_by_filter (specs : Spec.Set.t)
      ~(predicate : Property.t Blang.t) :
      Spec.With_id.t list
      * (Spec.With_id.t, Lookup_listing.Disable.t) List.Assoc.t =
    Ac.Spec.Set.partition_map specs ~f:(fun spec ->
        if Property.eval_b spec predicate then First spec
        else
          Second
            ( spec
            , Lookup_listing.Disable.make ~location:Machine ~reason:Filtered
            ))

  let partition_by_enabled (specs : Spec.With_id.t list) :
      Spec.With_id.t list
      * (Spec.With_id.t, Lookup_listing.Disable.t) List.Assoc.t =
    List.partition_map specs ~f:(fun m_spec ->
        if Spec.With_id.is_enabled m_spec then First m_spec
        else
          Second
            ( m_spec
            , Lookup_listing.Disable.make ~location:Machine ~reason:In_config
            ))

  let filtered_list ?(predicate : Property.t Blang.t = Blang.true_)
      (specs : Spec.Set.t) : Spec.t Lookup_listing.t =
    let pass_filter, fail_filter = partition_by_filter specs ~predicate in
    let enabled_machines, disabled_in_config =
      partition_by_enabled pass_filter
    in
    (* [of_list] errors only if there are duplicate IDs; as we're reducing an
       existing table, it should be safe to assume there are no errors. *)
    let enabled_table =
      Or_error.ok_exn (Ac.Spec.Set.of_list enabled_machines)
    in
    Lookup_listing.make enabled_table
      ~disabled:(fail_filter @ disabled_in_config)
end

let partition_by_filter_gen (type spec qspec) (specs : spec Ac.Spec.Set.t)
    ~(filter : spec Ac.Spec.With_id.t -> bool)
    ~(qualify : spec Ac.Spec.With_id.t -> qspec) :
    qspec list * (qspec, Lookup_listing.Disable.t) List.Assoc.t =
  Ac.Spec.Set.partition_map specs ~f:(fun spec ->
      if filter spec then First (qualify spec)
      else
        Second
          ( qualify spec
          , Lookup_listing.Disable.make ~location:Spec ~reason:Filtered ))

let partition_by_enabled_gen (type spec) (specs : spec list)
    ~(is_enabled : spec -> bool) :
    spec list * (spec, Lookup_listing.Disable.t) List.Assoc.t =
  List.partition_map specs ~f:(fun spec ->
      if is_enabled spec then First spec
      else
        Second
          (spec, Lookup_listing.Disable.make ~location:Spec ~reason:In_config))

(** The flow for both compiler and backend lookup is basically the same, with
    some different types and intermediate functions. As such, we build it
    with a functor. *)
module Make (B : sig
  type spec

  type pred

  val id_type : string

  val specs_of_machine : Spec.t Ac.Spec.With_id.t -> spec Ac.Spec.Set.t

  val eval_pred : pred -> spec Ac.Spec.With_id.t -> bool

  val is_enabled : spec -> bool

  val test_spec : spec Qualified.t -> unit Or_error.t
end) =
struct
  let is_enabled (q : B.spec Qualified.t) : bool =
    q |> Qualified.spec |> Act_common.Spec.With_id.spec |> B.is_enabled

  let specs_of_enabled_machine ?(predicate : B.pred option)
      (m_spec : Spec.t Ac.Spec.With_id.t) :
      B.spec Qualified.t list
      * (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t =
    let qualify spec = Qualified.make ~m_spec ~spec in
    let filter spec =
      Option.for_all ~f:(Fn.flip B.eval_pred spec) predicate
    in
    let specs = B.specs_of_machine m_spec in
    let pass_filter, fail_filter =
      partition_by_filter_gen specs ~qualify ~filter
    in
    let enabled_compilers, disabled_in_config =
      partition_by_enabled_gen pass_filter ~is_enabled
    in
    (enabled_compilers, fail_filter @ disabled_in_config)

  let specs_of_enabled_machines ?(predicate : B.pred option)
      (machines : Spec.Set.t) :
      B.spec Qualified.t list
      * (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t =
    let par =
      Act_common.Spec.Set.map machines
        ~f:(specs_of_enabled_machine ?predicate)
    in
    let en, di = List.unzip par in
    (List.join en, List.join di)

  let machine_disabled_specs
      (disabled_machines :
        (Spec.With_id.t, Lookup_listing.Disable.t) List.Assoc.t) :
      (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t =
    List.concat_map disabled_machines ~f:(fun (m_spec, disable) ->
        m_spec |> B.specs_of_machine
        |> Ac.Spec.Set.map ~f:(fun spec ->
               (Qualified.make ~m_spec ~spec, disable)))

  let actually_test_specs :
         B.spec Qualified.t list
      -> B.spec Qualified.t list
         * (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t =
    List.partition_map ~f:(fun spec ->
        match B.test_spec spec with
        | Ok () ->
            First spec
        | Error error ->
            Second
              (spec, Lookup_listing.Disable.make_failed ~location:Spec ~error))

  let maybe_test_specs ?(test_specs : bool = false)
      (specs : B.spec Qualified.t list) :
      B.spec Qualified.t list
      * (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t =
    if test_specs then actually_test_specs specs else (specs, [])

  let add_fqids_to_enabled :
      B.spec Qualified.t list -> B.spec Qualified.t Ac.Spec.With_id.t list =
    List.map ~f:(fun s ->
        Ac.Spec.With_id.make ~id:(Qualified.fqid s) ~spec:s)

  let add_fqids_to_disabled :
         (B.spec Qualified.t, Lookup_listing.Disable.t) List.Assoc.t
      -> ( B.spec Qualified.t Ac.Spec.With_id.t
         , Lookup_listing.Disable.t )
         List.Assoc.t =
    List.map ~f:(fun (s, e) ->
        (Ac.Spec.With_id.make ~id:(Qualified.fqid s) ~spec:s, e))

  let filtered_list ?(machine_predicate : Property.t Blang.t option)
      ?(predicate : B.pred option) ?(test_specs : bool option)
      (specs : Spec.Set.t) : B.spec Qualified.t Lookup_listing.t Or_error.t =
    let machine_listing =
      Machine.filtered_list ?predicate:machine_predicate specs
    in
    let enabled_machines = Lookup_listing.enabled machine_listing in
    let disabled_machines = Lookup_listing.disabled machine_listing in
    let machine_disabled_specs = machine_disabled_specs disabled_machines in
    let enabled_specs, disabled_specs =
      specs_of_enabled_machines enabled_machines ?predicate
    in
    let passed_specs, failed_test =
      maybe_test_specs enabled_specs ?test_specs
    in
    let fqid_passed_specs = add_fqids_to_enabled passed_specs in
    (* There may be duplicate FQIDs even if there aren't any duplicate
       machines or specs.. *)
    Or_error.Let_syntax.(
      let%map passed_set = Ac.Spec.Set.of_list fqid_passed_specs in
      let disabled =
        add_fqids_to_disabled
          (machine_disabled_specs @ disabled_specs @ failed_test)
      in
      Lookup_listing.make passed_set ~disabled)

  let lookup_single ?(default_machines : Act_common.Id.t list option)
      ?(machine_predicate : Property.t Blang.t option)
      ?(predicate : B.pred option) ?(test_specs : bool option)
      (specs : Spec.Set.t) ~(fqid : Ac.Id.t) : B.spec Qualified.t Or_error.t
      =
    (* TODO(@MattWindsor91): there may be a more efficient way of doing this
       than constructing a whole lookup listing. *)
    Or_error.Let_syntax.(
      let%bind listing =
        filtered_list ?machine_predicate ?predicate ?test_specs specs
      in
      Lookup_listing.get_with_fqid ?default_machines ~id_type:B.id_type ~fqid
        listing)
end

module Backend (Basic : sig
  val test : Qualified.Backend.t -> unit Or_error.t
end) : Lookup_types.S_backend = Make (struct
  type spec = Act_backend.Spec.t

  type pred = Act_backend.Property.t Blang.t

  let id_type = "backend"

  let is_enabled = Act_backend.Spec.is_enabled

  let eval_pred = Act_backend.Property.eval_b

  let specs_of_machine = Fn.compose Spec.backends Spec.With_id.spec

  let test_spec = Basic.test
end)
