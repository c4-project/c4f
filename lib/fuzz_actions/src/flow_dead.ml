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
  let prefix_name (rest : Common.Id.t) : Common.Id.t =
    prefix_name Common.Id.("insert" @: rest)

  let goto_name = prefix_name Common.Id.("goto" @: empty)

  module Early_out_payload = struct
    type t = {
      if_cond: Fir.Expression.t option
    ; kind: Fir.Early_out.t
    } [@@deriving sexp]

    module type S_action = Fuzz.Action_types.S with type Payload.t = t Fuzz.Payload_impl.Pathed.t

    let gen' (kind_filter : Fir.Early_out.t -> bool) (gen_cond: Fir.Expression.t option Fuzz.Payload_gen.t): t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* if_cond = gen_cond in
        let+ kind = lift_quickcheck (Q.Generator.filter Fir.Early_out.quickcheck_generator ~f:kind_filter) in
        { if_cond; kind }
      )

    let gen (path_filter : Fuzz.Availability.Context.t -> Fuzz.Path_filter.t)
    (kind_filter: Fuzz.Path.Flagged.t -> Fir.Early_out.t -> bool)
    (gen_cond : Fuzz.Path.Flagged.t -> Fir.Expression.t option Fuzz.Payload_gen.t) : t Fuzz.Payload_impl.Pathed.t Fuzz.Payload_gen.t =
      Fuzz.Payload_impl.Pathed.gen Insert path_filter
        (fun path -> gen' (kind_filter path) (gen_cond path))

    let construct ({kind; if_cond} : t) : Fuzz.Subject.Statement.t =
      let eo =
        Fuzz.Subject.Statement.make_generated_prim
          (Accessor.construct Fir.Prim_statement.early_out kind)
      in
      match if_cond with
      | None -> eo
      | Some cond ->
        let t_branch =
          Fuzz.Subject.Block.make_generated ~statements:[eo] () in
        let f_branch =
          Fuzz.Subject.Block.make_dead_code () in
        Accessor.construct Fir.Statement.if_stm
          (Fir.If.make ~cond ~t_branch ~f_branch)
  end

  module Early_out : Early_out_payload.S_action = struct
    let name = prefix_name Common.Id.("early-out" @: empty)

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Inserts a valid 'early-out' statement (break, continue, or return)
           into a random dead-code location. |}

    let base_path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.require_flag In_dead_code

    let path_filter (_ : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t =
      base_path_filter

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = struct
      type t = Early_out_payload.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

      let kind_pred ({flags; _} : Fuzz.Path.Flagged.t) :
          Fir.Early_out.t -> bool =
        if Set.mem flags In_loop then Fn.const true
        else Fn.non Fir.Early_out.in_loop_only

      let gen : t Fuzz.Payload_gen.t =
        Early_out_payload.gen path_filter
          kind_pred
          (fun _ -> Fuzz.Payload_gen.return None)
    end

    (* In generation, we pick the path first and then work out which
       early-out to apply; when checking the path, we go backwards and assert
       that the path must meet the filtering needs of the early-out. *)

    let kind_filter (kind : Fir.Early_out.t) : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        add_if base_path_filter
          ~when_:(Fir.Early_out.in_loop_only kind)
          ~add:(require_flag In_loop))

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.Payload_impl.Pathed.insert
        ~filter:(kind_filter payload.payload.kind)
        payload ~test ~f:Early_out_payload.construct
  end

  module Early_out_loop_end :
    Early_out_payload.S_action = struct
    let name = prefix_name Common.Id.("early-out-loop-end" @: empty)

    let base_path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(require_flag In_loop + anchor Bottom)

    let readme () =
      Act_utils.My_string.format_for_readme
        {| Inserts a continue (or break, if semantically appropriate) onto the
           end of a loop, and marks the area afterwards as dead code. |}

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = struct
      type t = Early_out_payload.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

      let kind_pred ({flags; _} : Fuzz.Path.Flagged.t) :
          Fir.Early_out.t -> bool =
        (* We can only break at the end of an arbitrary loop if we know that
           it can only be executed once, and it's never safe to return from
           an arbitrary part of live code.

           TODO(@MattWindsor91): contemplate a top-level early-out that does
           what this action does, but inserts returns. *)
        function
        | Continue ->
            true
        | Break ->
            not (Set.mem flags In_execute_multi)
        | Return ->
            false

      let path_filter (_ : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t
          =
        base_path_filter

      let gen_cond' : Fuzz.Path.Flagged.t -> Fir.Expression.t Fuzz.Payload_gen.t =
          Staged.unstage (Fuzz.Payload_impl.Cond_pathed.lift_cond_gen Fir_gen.Expr.tautology)

      let gen_cond (p : Fuzz.Path.Flagged.t) : Fir.Expression.t option Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.(
          let* f = flag Fuzz.Config_tables.wrap_early_out_flag in
          if f then map (gen_cond' p) ~f:Option.some else return None
        )

      let gen : t Fuzz.Payload_gen.t =
        Early_out_payload.gen path_filter
          kind_pred
          gen_cond
    end

    (* As with Early_out, we're working backwards here! *)

    let kind_filter (kind : Fir.Early_out.t) : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        add_if base_path_filter
          ~when_:(Fir.Early_out.equal kind Break)
          ~add:(forbid_flag In_execute_multi))

    let make_contraption (pld : Early_out_payload.t) :
        Fuzz.Subject.Statement.t list =
      [ Early_out_payload.construct pld
      ; Accessor.construct Fir.Statement.flow
          (Fir.Flow_block.implicit (Fuzz.Subject.Block.make_dead_code ())) ]

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      (* We can't use Pathed.insert, because we're inserting multiple
         statements. *)
      Fuzz.State.Monad.Monadic.return
        (Fuzz.Path_consumers.consume_with_flags test ~path:payload.where
           ~filter:(kind_filter payload.payload.kind)
           ~action:(Insert (make_contraption payload.payload)))
  end

  module Goto :
    Fuzz.Action_types.S
      with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Pathed.t = struct
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
        require_flag In_dead_code + in_threads_only threads_with_labels)

    let path_filter (ctx : Fuzz.Availability.Context.t) : Fuzz.Path_filter.t
        =
      ctx |> Fuzz.Availability.Context.state |> Fuzz.State.labels
      |> path_filter'

    module Payload = struct
      type t = Common.C_id.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

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

      let gen' (ins_path : Fuzz.Path.Flagged.t) :
          Common.C_id.t Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.(
          let* labels = lift (Fn.compose Fuzz.State.labels Context.state) in
          let labels_in_tid = reachable_labels labels ins_path in
          lift_quickcheck (Base_quickcheck.Generator.of_list labels_in_tid))

      let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
    end

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        M.(
          lift (fun ctx ->
              ctx |> Fuzz.Availability.Context.state |> Fuzz.State.labels)
          >>| path_filter'
          >>= Fuzz.Availability.is_filter_constructible ~kind:Insert))

    let make_goto (id : Common.C_id.t) : Fuzz.Subject.Statement.t =
      id
      |> Accessor.construct Fir.Prim_statement.goto
      |> Fuzz.Subject.Statement.make_generated_prim

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Let_syntax.(
          let%bind filter = with_labels path_filter' in
          Fuzz.Payload_impl.Pathed.insert ~filter payload ~test ~f:make_goto))
  end
end
