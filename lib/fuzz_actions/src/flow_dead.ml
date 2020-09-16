(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("flow" @: "dead" @: rest)

module Insert = struct
  let early_out_name =
    prefix_name Common.Id.("insert" @: "early-out" @: empty)

  let goto_name = prefix_name Common.Id.("insert" @: "goto" @: empty)

  module Early_out_payload = struct
    (* TODO(@MattWindsor91): We can't easily refactor this into an
       Insertion.Make payload, because the path filter depends on the choice
       of early-out. We'd have to split it into three different actions
       first. *)

    type t = Fir.Early_out.t Fuzz.Payload_impl.Insertion.t [@@deriving sexp]

    let quickcheck_path (test : Fuzz.Subject.Test.t) :
        Fuzz.Path.Flagged.t Fuzz.Opt_gen.t =
      let filter = Fuzz.Path_filter.(in_dead_code_only empty) in
      Fuzz.Path_producers.try_gen_with_flags test ~filter ~kind:Insert

    let kind_pred ({flags; _} : Fuzz.Path.Flagged.t) :
        Fir.Early_out.t -> bool =
      if Set.mem flags In_loop then Fn.const true
      else Fn.non Fir.Early_out.in_loop_only

    let quickcheck_early_out (path : Fuzz.Path.Flagged.t) :
        Fir.Early_out.t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.(
        filter Fir.Early_out.quickcheck_generator ~f:(kind_pred path))

    let gen : t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* subject = lift Context.subject in
        let* where = lift_opt_gen (quickcheck_path subject) in
        let+ to_insert = lift_quickcheck (quickcheck_early_out where) in
        Fuzz.Payload_impl.Insertion.make ~where ~to_insert)
  end

  module Early_out :
    Fuzz.Action_types.S with type Payload.t = Early_out_payload.t = struct
    let name = early_out_name

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Inserts a valid 'early-out' statement (break or return) into a random
         dead-code location. |}

    let base_path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(in_dead_code_only empty)

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = Early_out_payload

    let kind_filter (kind : Fir.Early_out.t) :
        (Fuzz.Path_filter.t -> Fuzz.Path_filter.t) Staged.t =
      Staged.stage
        ( if Fir.Early_out.in_loop_only kind then
          Fuzz.Path_filter.in_loop_only
        else Fn.id )

    let make_early_out (kind : Fir.Early_out.t) : Fuzz.Subject.Statement.t =
      Fuzz.Subject.Statement.make_generated_prim
        (Accessor.construct Fir.Prim_statement.early_out kind)

    let run_inner (test : Fuzz.Subject.Test.t) (path : Fuzz.Path.Flagged.t)
        (kind : Fir.Early_out.t) : Fuzz.Subject.Test.t Or_error.t =
      let f = Staged.unstage (kind_filter kind) in
      let filter = f base_path_filter in
      Fuzz.Path_consumers.consume_with_flags test ~filter ~path
        ~action:(Insert [make_early_out kind])

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      let path = Fuzz.Payload_impl.Insertion.where payload in
      let kind = Fuzz.Payload_impl.Insertion.to_insert payload in
      Fuzz.State.Monad.Monadic.return (run_inner test path kind)
  end

  module Goto :
    Fuzz.Action_types.S
      with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Insertion.t =
  struct
    let name = goto_name

    let readme () : string =
      Act_utils.My_string.format_for_readme
        {|
        Inserts a jump to a random thread-local label inside a dead-code block.

        This action only fires in dead-code blocks for which there are available
        labels in the same thread; it does not jump outside the thread.
      |}

    let path_filter' (labels : Set.M(Common.Litmus_id).t) :
        Fuzz.Path_filter.t =
      let threads_with_labels =
        Set.filter_map (module Int) ~f:Common.Litmus_id.tid labels
      in
      Fuzz.Path_filter.(
        empty |> in_dead_code_only
        |> Fuzz.Path_filter.in_threads_only ~threads:threads_with_labels)

    let path_filter (ctx : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t
        =
      ctx |> Fuzz.Availability.Context.state |> Fuzz.State.labels
      |> path_filter'

    module Payload = Fuzz.Payload_impl.Insertion.Make (struct
      type t = Act_common.C_id.t [@@deriving sexp]

      let path_filter = path_filter

      let reachable_labels (labels : Set.M(Common.Litmus_id).t)
          ({path; _} : Fuzz.Path.Flagged.t) : Common.C_id.t list =
        let from = Fuzz.Path.tid path in
        labels
        |> Set.filter_map
             (module Common.C_id)
             ~f:(fun x ->
               Option.some_if
                 (Common.Litmus_id.is_in_local_scope x ~from)
                 (Common.Litmus_id.variable_name x))
        |> Set.to_list

      let gen (ins_path : Fuzz.Path.Flagged.t) : t Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.(
          let* labels = lift (Fn.compose Fuzz.State.labels Context.state) in
          let labels_in_tid = reachable_labels labels ins_path in
          lift_quickcheck (Base_quickcheck.Generator.of_list labels_in_tid))
    end)

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        M.(
          lift (fun ctx ->
              ctx |> Fuzz.Availability.Context.state |> Fuzz.State.labels)
          >>| path_filter'
          >>= Fuzz.Availability.is_filter_constructible ~kind:Insert))

    let run (subject : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      let path = Fuzz.Payload_impl.Insertion.where payload in
      let label = Fuzz.Payload_impl.Insertion.to_insert payload in
      let goto_stm =
        label
        |> Accessor.construct Fir.Prim_statement.goto
        |> Fuzz.Subject.Statement.make_generated_prim
      in
      Fuzz.State.Monad.(
        Let_syntax.(
          let%bind filter = with_labels path_filter' in
          Monadic.return
            (Fuzz.Path_consumers.consume_with_flags subject ~filter ~path
               ~action:(Insert [goto_stm]))))
  end
end
