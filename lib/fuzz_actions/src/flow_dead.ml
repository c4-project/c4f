(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let prefix_name (rest : Common.Id.t) : Common.Id.t =
  Common.Id.("dead" @: rest)

module Insert = struct
  let prefix_name (rest : Common.Id.t) : Common.Id.t =
    prefix_name Common.Id.("insert" @: rest)

  let goto_name = prefix_name Common.Id.("goto" @: empty)

  module Early_out_payload = struct
    type t = {if_cond: Fir.Expression.t option; kind: Fir.Early_out.t}
    [@@deriving sexp]

    module type S_action =
      Fuzz.Action_types.S with type Payload.t = t Fuzz.Payload_impl.Pathed.t

    let gen' (kind_filter : Fir.Early_out.t -> bool)
        (gen_cond : Fir.Expression.t option Fuzz.Payload_gen.t) :
        t Fuzz.Payload_gen.t =
      Fuzz.Payload_gen.(
        let* if_cond = gen_cond in
        let+ kind =
          lift_quickcheck
            (Q.Generator.filter Fir.Early_out.quickcheck_generator
               ~f:kind_filter )
        in
        {if_cond; kind})

    let gen (path_filter : Fuzz.State.t -> Fuzz.Path_filter.t)
        (kind_filter : Fuzz.Path.With_meta.t -> Fir.Early_out.t -> bool)
        (gen_cond :
          Fuzz.Path.With_meta.t -> Fir.Expression.t option Fuzz.Payload_gen.t
          ) : t Fuzz.Payload_impl.Pathed.t Fuzz.Payload_gen.t =
      Fuzz.Payload_impl.Pathed.gen Insert path_filter (fun path ->
          gen' (kind_filter path) (gen_cond path) )

    let construct ({kind; if_cond} : t) : Fuzz.Subject.Statement.t =
      let eo =
        Fuzz.Subject.Statement.make_generated_prim
          (Accessor.construct Fir.Prim_statement.early_out kind)
      in
      match if_cond with
      | None -> eo
      | Some cond ->
          let t_branch =
            Fuzz.Subject.Block.make_generated ~statements:[eo] ()
          in
          let f_branch = Fuzz.Subject.Block.make_dead_code () in
          Accessor.construct Fir.Statement.if_stm
            (Fir.If.make ~cond ~t_branch ~f_branch)
  end

  module Early_out : Early_out_payload.S_action = struct
    let name = prefix_name Common.Id.("early-out" @: empty)

    let readme =
      lazy
        {| Inserts a valid 'early-out' statement (break, continue, or return)
           into a random dead-code location. |}

    let base_path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.require_flag In_dead_code

    let path_filter (_ : Fuzz.State.t) : Fuzz.Path_filter.t =
      base_path_filter

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = struct
      type t = Early_out_payload.t Fuzz.Payload_impl.Pathed.t
      [@@deriving sexp]

      let kind_pred (path : Fuzz.Path.With_meta.t) : Fir.Early_out.t -> bool
          =
        if path.@(Fuzz.Path.With_meta.flag In_loop) then Fn.const true
        else Fn.non Fir.Early_out.in_loop_only

      let gen : t Fuzz.Payload_gen.t =
        Early_out_payload.gen path_filter kind_pred (fun _ ->
            Fuzz.Payload_gen.return None )
    end

    let recommendations (_ : Payload.t) : Common.Id.t list = []

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

  module Early_out_loop_end : Early_out_payload.S_action = struct
    let name = prefix_name Common.Id.("early-out-loop-end" @: empty)

    let base_path_filter : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        ends_in_block (Flow (Some (Loop None))) + anchor Bottom)

    let readme =
      lazy
        {| Inserts a continue (or break, if semantically appropriate) onto the
           end of a loop, and marks the area afterwards as dead code. |}

    let available : Fuzz.Availability.t =
      Fuzz.Availability.is_filter_constructible base_path_filter ~kind:Insert

    module Payload = struct
      type t = Early_out_payload.t Fuzz.Payload_impl.Pathed.t
      [@@deriving sexp]

      let kind_pred (path : Fuzz.Path.With_meta.t) : Fir.Early_out.t -> bool
          =
        (* We can only break at the end of a populated loop if we know that
           it can only be executed once, and it's never safe to return from
           an arbitrary part of live code.

           TODO(@MattWindsor91): contemplate a top-level early-out that does
           what this action does, but inserts returns. *)
        function
        | Continue -> true
        | Break ->
            (* We should already be in a bottom anchor, so this is equivalent
               to checking for full, which, in turn, tells us that the loop
               is unpopulated. *)
            path.@(Fuzz.Path_meta.(With_meta.meta @> anchor @> Anchor.top))
            || not path.@(Fuzz.Path.With_meta.flag In_execute_multi)
        | Return -> false

      let path_filter (_ : Fuzz.State.t) : Fuzz.Path_filter.t =
        base_path_filter

      let gen_cond' :
          Fuzz.Path.With_meta.t -> Fir.Expression.t Fuzz.Payload_gen.t =
        Staged.unstage
          (Fuzz.Payload_impl.Cond_pathed.lift_cond_gen Fir_gen.Expr.tautology)

      let gen_cond (p : Fuzz.Path.With_meta.t) :
          Fir.Expression.t option Fuzz.Payload_gen.t =
        Fuzz.Payload_gen.(
          let* f = flag Fuzz.Config_tables.wrap_early_out_flag in
          if f then map (gen_cond' p) ~f:Option.some else return None)

      let gen : t Fuzz.Payload_gen.t =
        Early_out_payload.gen path_filter kind_pred gen_cond
    end

    let recommendations (_ : Payload.t) : Common.Id.t list = []

    (* As with Early_out, we're working backwards here, trying to make sure
       the conditions where break is not valid get checked. We don't have any
       ability to disjoin filters, so this is a bit of a hack; hopefully the
       actual metadata carried in the path performs a stronger check.

       TODO(@MattWindsor91): consider disjunction of filters. *)

    let kind_filter (anc : Fuzz.Path_meta.Anchor.t option)
        (kind : Fir.Early_out.t) : Fuzz.Path_filter.t =
      Fuzz.Path_filter.(
        add_if base_path_filter
          ~when_:
            ( Fir.Early_out.equal kind Break
            && not anc.@(Fuzz.Path_meta.Anchor.top) )
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
      Fuzz.State.Monad.(
        add_expression_dependencies_at_path
          (Option.to_list payload.payload.if_cond)
          ~path:payload.where
        >>= fun () ->
        Monadic.return
          (Fuzz.Path_consumers.consume test ~path:payload.where
             ~filter:
               (kind_filter payload.where.meta.anchor payload.payload.kind)
             ~action:(Insert (make_contraption payload.payload)) ))
  end

  module Goto :
    Fuzz.Action_types.S
      with type Payload.t = Common.C_id.t Fuzz.Payload_impl.Pathed.t = struct
    let name = goto_name

    let readme : string Lazy.t =
      lazy
        {|
        Inserts a jump to a random thread-local label inside a dead-code block.

        This action only fires in dead-code blocks for which there are available
        labels in the same thread; it does not jump outside the thread.
      |}

    let path_filter ({labels; _} : Fuzz.State.t) : Fuzz.Path_filter.t =
      let threads_with_labels =
        Set.filter_map (module Int) ~f:Common.Litmus_id.tid labels
      in
      Fuzz.Path_filter.(
        require_flag In_dead_code + in_threads_only threads_with_labels)

    module Payload = struct
      type t = Common.C_id.t Fuzz.Payload_impl.Pathed.t [@@deriving sexp]

      let reachable_labels (labels : Set.M(Common.Litmus_id).t)
          ({path; _} : Fuzz.Path.With_meta.t) : Common.C_id.t list =
        let from = Fuzz.Path.tid path in
        labels
        |> Set.filter_map
             (module Common.C_id)
             ~f:(fun x ->
               Option.some_if
                 (Common.Litmus_id.is_in_local_scope x ~from)
                 (Common.Litmus_id.variable_name x) )
        |> Set.to_list

      let gen' (ins_path : Fuzz.Path.With_meta.t) :
          Common.C_id.t Fuzz.Payload_gen.t =
        Fuzz.(
          Payload_gen.(
            let* labels = lift_acc (Context.state @> Fuzz.State.labels) in
            let labels_in_tid = reachable_labels labels ins_path in
            lift_quickcheck (Base_quickcheck.Generator.of_list labels_in_tid)))

      let gen = Fuzz.Payload_impl.Pathed.gen Insert path_filter gen'
    end

    let recommendations (_ : Payload.t) : Common.Id.t list = []

    let available : Fuzz.Availability.t =
      Fuzz.Availability.(
        M.(
          lift_state path_filter
          >>= Fuzz.Availability.is_filter_constructible ~kind:Insert))

    let make_goto (id : Common.C_id.t) : Fuzz.Subject.Statement.t =
      id
      |> Accessor.construct Fir.Prim_statement.goto
      |> Fuzz.Subject.Statement.make_generated_prim

    let run (test : Fuzz.Subject.Test.t) ~(payload : Payload.t) :
        Fuzz.Subject.Test.t Fuzz.State.Monad.t =
      Fuzz.State.Monad.(
        Let_syntax.(
          let%bind filter = peek path_filter in
          Fuzz.Payload_impl.Pathed.insert ~filter payload ~test ~f:make_goto))
  end
end
