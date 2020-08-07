(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module F = Act_fuzz
end

let prefix_name (rest : Ac.Id.t) : Ac.Id.t = Ac.Id.("flow" @: "dead" @: rest)

module Insert = struct
  let early_out_name = prefix_name Ac.Id.("insert" @: "early-out" @: empty)

  let goto_name = prefix_name Ac.Id.("insert" @: "goto" @: empty)

  module Early_out_payload = struct
    (* TODO(@MattWindsor91): We can't easily refactor this into an
       Insertion.Make payload, because the path filter depends on the choice
       of early-out. We'd have to split it into three different actions
       first. *)

    type t = Fir.Early_out.t F.Payload_impl.Insertion.t [@@deriving sexp]

    let quickcheck_path (test : F.Subject.Test.t) :
        F.Path.Flagged.t F.Opt_gen.t =
      let filter = F.Path_filter.(in_dead_code_only empty) in
      F.Path_producers.try_gen_with_flags test ~filter ~kind:Insert

    let kind_pred (path : F.Path.Flagged.t) : Fir.Early_out.t -> bool =
      let flags = F.Path_flag.Flagged.flags path in
      if Set.mem flags In_loop then Fn.const true
      else Fn.non Fir.Early_out.in_loop_only

    let quickcheck_early_out (path : F.Path.Flagged.t) :
        Fir.Early_out.t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.(
        filter Fir.Early_out.quickcheck_generator ~f:(kind_pred path))

    let gen : t F.Payload_gen.t =
      F.Payload_gen.(
        let* subject = lift Context.subject in
        let* where = lift_opt_gen (quickcheck_path subject) in
        let+ to_insert = lift_quickcheck (quickcheck_early_out where) in
        F.Payload_impl.Insertion.make ~where ~to_insert)
  end

  module Early_out :
    F.Action_types.S with type Payload.t = Early_out_payload.t = struct
    let name = early_out_name

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Inserts a valid 'early-out' statement (break or return) into a random
         dead-code location. |}

    let base_path_filter : F.Path_filter.t =
      F.Path_filter.(in_dead_code_only empty)

    let available : F.Availability.t =
      F.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = Early_out_payload

    let kind_filter (kind : Fir.Early_out.t) :
        (F.Path_filter.t -> F.Path_filter.t) Staged.t =
      Staged.stage
        ( if Fir.Early_out.in_loop_only kind then F.Path_filter.in_loop_only
        else Fn.id )

    let make_early_out (kind : Fir.Early_out.t) : F.Subject.Statement.t =
      Fir.(
        Statement.prim F.Metadata.generated (Prim_statement.early_out kind))

    let run_inner (test : F.Subject.Test.t) (path : F.Path.Flagged.t)
        (kind : Fir.Early_out.t) : F.Subject.Test.t Or_error.t =
      let f = Staged.unstage (kind_filter kind) in
      let filter = f base_path_filter in
      F.Path_consumers.consume_with_flags test ~filter ~path
        ~action:(Insert [make_early_out kind])

    let run (test : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      let path = F.Payload_impl.Insertion.where payload in
      let kind = F.Payload_impl.Insertion.to_insert payload in
      F.State.Monad.Monadic.return (run_inner test path kind)
  end

  module Goto :
    F.Action_types.S
      with type Payload.t = Ac.C_id.t F.Payload_impl.Insertion.t = struct
    let name = goto_name

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {|
        Inserts a jump to a random thread-local label inside a dead-code block.

        This action only fires in dead-code blocks for which there are available
        labels in the same thread; it does not jump outside the thread.
      |}

    let path_filter' (labels : Set.M(Ac.Litmus_id).t) : F.Path_filter.t =
      let threads_with_labels =
        Set.filter_map (module Int) ~f:Ac.Litmus_id.tid labels
      in
      F.Path_filter.(
        empty |> in_dead_code_only
        |> F.Path_filter.in_threads_only ~threads:threads_with_labels)

    let path_filter (ctx : F.Availability.Context.t) : F.Path_filter.t =
      ctx |> F.Availability.Context.state |> F.State.labels |> path_filter'

    module Payload = F.Payload_impl.Insertion.Make (struct
      type t = Act_common.C_id.t [@@deriving sexp]

      let path_filter = path_filter

      let reachable_labels (labels : Set.M(Ac.Litmus_id).t)
          (path : F.Path.Flagged.t) : Ac.C_id.t list =
        let from = F.Path.tid (F.Path_flag.Flagged.path path) in
        labels
        |> Set.filter_map
             (module Ac.C_id)
             ~f:(fun x ->
               Option.some_if
                 (Ac.Litmus_id.is_in_local_scope x ~from)
                 (Ac.Litmus_id.variable_name x))
        |> Set.to_list

      let gen (ins_path : F.Path.Flagged.t) : t F.Payload_gen.t =
        F.Payload_gen.(
          let* labels = lift (Fn.compose F.State.labels Context.state) in
          let labels_in_tid = reachable_labels labels ins_path in
          lift_quickcheck (Base_quickcheck.Generator.of_list labels_in_tid))
    end)

    let available (ctx : F.Availability.Context.t) : bool Or_error.t =
      let labels = ctx |> F.Availability.Context.state |> F.State.labels in
      F.Availability.is_filter_constructible (path_filter' labels)
        ~kind:Insert ctx

    let run (subject : F.Subject.Test.t) ~(payload : Payload.t) :
        F.Subject.Test.t F.State.Monad.t =
      let path = F.Payload_impl.Insertion.where payload in
      let label = F.Payload_impl.Insertion.to_insert payload in
      let goto_stm =
        Fir.(
          label |> Prim_statement.goto |> Statement.prim F.Metadata.generated)
      in
      F.State.Monad.(
        Let_syntax.(
          let%bind filter = with_labels path_filter' in
          Monadic.return
            (F.Path_consumers.consume_with_flags subject ~filter ~path
               ~action:(Insert [goto_stm]))))
  end
end
