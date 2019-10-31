(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module C_instance = Act_compiler.Instance
module C_instance_t = Act_compiler.Instance_types
module C_filter = Act_compiler.Filter

module Machine = struct
  let partition_by_filter (specs : Spec.Set.t)
      ~(predicate : Property.t Blang.t) :
      Spec.With_id.t list
      * (Spec.With_id.t, Resolver_listing.Disable.t) List.Assoc.t =
    Act_common.Spec.Set.partition_map specs ~f:(fun spec ->
        if Property.eval_b spec predicate then `Fst spec
        else
          `Snd
            ( spec
            , Resolver_listing.Disable.make ~location:Machine
                ~reason:Filtered ))

  let partition_by_enabled (specs : Spec.With_id.t list) :
      Spec.With_id.t list
      * (Spec.With_id.t, Resolver_listing.Disable.t) List.Assoc.t =
    List.partition_map specs ~f:(fun m_spec ->
        if Spec.With_id.is_enabled m_spec then `Fst m_spec
        else
          `Snd
            ( m_spec
            , Resolver_listing.Disable.make ~location:Machine
                ~reason:In_config ))

  let filtered_list ?(predicate : Property.t Blang.t = Blang.true_)
      (specs : Spec.Set.t) : Spec.t Resolver_listing.t =
    let pass_filter, fail_filter = partition_by_filter specs ~predicate in
    let enabled_machines, disabled_in_config =
      partition_by_enabled pass_filter
    in
    (* [of_list] errors only if there are duplicate IDs; as we're reducing
       an existing table, it should be safe to assume there are no errors. *)
    let enabled_table =
      Or_error.ok_exn (Act_common.Spec.Set.of_list enabled_machines)
    in
    Resolver_listing.make enabled_table
      ~disabled:(fail_filter @ disabled_in_config)
end

module Compiler (Resolver : sig
  val f :
       Act_compiler.Spec.With_id.t
    -> (module Act_compiler.Instance_types.Basic) Or_error.t
end) : Resolver_types.S_compiler = struct
  module Qc = Qualified.Compiler

  let resolve (q_spec : Qc.t) : (module C_instance_t.S) Or_error.t =
    let c_spec = Qc.c_spec q_spec in
    let m_spec = Qc.m_spec q_spec in
    Or_error.Let_syntax.(
      let%map (module B : C_instance_t.Basic) = Resolver.f c_spec in
      let (module Runner) = Spec.With_id.runner m_spec in
      ( module C_instance.Make (struct
        let cspec = c_spec

        include B
        module Runner = Runner
      end) : C_instance_t.S ))

  let partition_by_filter (m_spec : Spec.With_id.t)
      (specs : Act_compiler.Spec.Set.t)
      ~(predicate : Act_compiler.Property.t Blang.t) :
      Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    let qc c_spec = Qc.make ~m_spec ~c_spec in
    Act_common.Spec.Set.partition_map specs ~f:(fun spec ->
        if Act_compiler.Property.eval_b spec predicate then `Fst (qc spec)
        else
          `Snd
            ( qc spec
            , Resolver_listing.Disable.make ~location:Spec ~reason:Filtered
            ))

  let partition_by_enabled (specs : Qc.t list) :
      Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    List.partition_map specs ~f:(fun c_spec ->
        if Act_compiler.Spec.With_id.is_enabled (Qc.c_spec c_spec) then
          `Fst c_spec
        else
          `Snd
            ( c_spec
            , Resolver_listing.Disable.make ~location:Spec ~reason:In_config
            ))

  let compilers_of_enabled_machine (m_spec : Spec.With_id.t)
      ~(compiler_predicate : Act_compiler.Property.t Blang.t) :
      Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    let specs = Spec.With_id.compilers m_spec in
    let pass_filter, fail_filter =
      partition_by_filter m_spec specs ~predicate:compiler_predicate
    in
    let enabled_compilers, disabled_in_config =
      partition_by_enabled pass_filter
    in
    (enabled_compilers, fail_filter @ disabled_in_config)

  let compilers_of_enabled_machines (machines : Spec.Set.t)
      ~(compiler_predicate : Act_compiler.Property.t Blang.t) :
      Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    let par =
      Spec.Set.map machines
        ~f:(compilers_of_enabled_machine ~compiler_predicate)
    in
    let en, di = List.unzip par in
    (List.join en, List.join di)

  let machine_disabled_compilers
      (disabled_machines :
        (Spec.With_id.t, Resolver_listing.Disable.t) List.Assoc.t) :
      (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    List.concat_map disabled_machines ~f:(fun (m_spec, disable) ->
        m_spec |> Spec.With_id.compilers
        |> Act_compiler.Spec.Set.map ~f:(fun c_spec ->
               (Qc.make ~c_spec ~m_spec, disable)))

  let test_compiler (compiler : Qc.t) : unit Or_error.t =
    Or_error.Let_syntax.(
      let%bind (module Compiler) = resolve compiler in
      Compiler.test ())

  let actually_test_compilers :
         Qc.t list
      -> Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    List.partition_map ~f:(fun compiler ->
        match test_compiler compiler with
        | Ok () ->
            `Fst compiler
        | Error error ->
            `Snd
              ( compiler
              , Resolver_listing.Disable.make_failed ~location:Spec ~error
              ))

  let maybe_test_compilers (compilers : Qc.t list) ~(test_compilers : bool)
      : Qc.t list * (Qc.t, Resolver_listing.Disable.t) List.Assoc.t =
    if test_compilers then actually_test_compilers compilers
    else (compilers, [])

  let fqid_of_qc (s : Qc.t) : Act_common.Id.t =
    Act_common.(Id.(Spec.With_id.(id (Qc.m_spec s) @. id (Qc.c_spec s))))

  let add_fqids_to_enabled :
      Qc.t list -> Qc.t Act_common.Spec.With_id.t list =
    List.map ~f:(fun s ->
        Act_common.Spec.With_id.make ~id:(fqid_of_qc s) ~spec:s)

  let add_fqids_to_disabled :
         (Qc.t, Resolver_listing.Disable.t) List.Assoc.t
      -> ( Qc.t Act_common.Spec.With_id.t
         , Resolver_listing.Disable.t )
         List.Assoc.t =
    List.map ~f:(fun (s, e) ->
        (Act_common.Spec.With_id.make ~id:(fqid_of_qc s) ~spec:s, e))

  let filtered_list ?(machine_predicate : Property.t Blang.t option)
      ?(compiler_predicate : Act_compiler.Property.t Blang.t = Blang.true_)
      ?(test_compilers : bool = false) (specs : Spec.Set.t) :
      Qc.t Resolver_listing.t =
    let machine_listing =
      Machine.filtered_list ?predicate:machine_predicate specs
    in
    let enabled_machines = Resolver_listing.enabled machine_listing in
    let disabled_machines = Resolver_listing.disabled machine_listing in
    let machine_disabled_compilers =
      machine_disabled_compilers disabled_machines
    in
    let enabled_compilers, disabled_compilers =
      compilers_of_enabled_machines enabled_machines ~compiler_predicate
    in
    let passed_compilers, failed_test =
      maybe_test_compilers enabled_compilers ~test_compilers
    in
    let fqid_passed_compilers = add_fqids_to_enabled passed_compilers in
    (* See the similar comment in [Machine]. *)
    let passed_set =
      Or_error.ok_exn (Act_common.Spec.Set.of_list fqid_passed_compilers)
    in
    let disabled =
      add_fqids_to_disabled
        (machine_disabled_compilers @ disabled_compilers @ failed_test)
    in
    Resolver_listing.make passed_set ~disabled
end
