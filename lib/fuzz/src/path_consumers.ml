(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
  module Fir = Act_fir
  module Stm = Fir.Statement_traverse
end

type 'a transformer = 'a -> 'a Or_error.t

module rec Statement :
  (Path_types.S_consumer
    with type t = Path.Stm.t
     and type target = Subject.Statement.t) = struct
  type t = Path.Stm.t

  module Bm = Stm.Base_map (Or_error)

  type target = Subject.Statement.t

  let invalid_target_error (kind : string) (dest : Subject.Statement.t) :
      'a Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for this kind of path" ~kind
          ~target:(Stm.erase_meta dest : unit Fir.Statement.t)]

  let in_if_error (dest : Subject.Statement.t) : 'a -> 'b Or_error.t =
    Fn.const (invalid_target_error "in_if" dest)

  let in_loop_error (dest : Subject.Statement.t) : 'a -> 'b Or_error.t =
    Fn.const (invalid_target_error "in_loop" dest)

  let handle_in_if (dest : Subject.Statement.t)
      ~(f :
         target:Subject.Statement.If.t -> Subject.Statement.If.t Or_error.t)
      : Subject.Statement.t Or_error.t =
    Bm.bmap dest
      ~if_stm:(fun target -> f ~target)
      ~flow:(in_if_error dest) ~prim:(in_if_error dest)

  let handle_in_flow (dest : Subject.Statement.t)
      ~(f :
            target:Subject.Statement.Flow.t
         -> Subject.Statement.Flow.t Or_error.t) :
      Subject.Statement.t Or_error.t =
    Bm.bmap dest
      ~flow:(fun target -> f ~target)
      ~if_stm:(in_loop_error dest) ~prim:(in_loop_error dest)

  let handle_path (path : Path.Stm.t)
      ~(if_stm :
            Path.If.t
         -> target:Subject.Statement.If.t
         -> Subject.Statement.If.t Or_error.t)
      ~(flow :
            Path.Flow.t
         -> target:Subject.Statement.Flow.t
         -> Subject.Statement.Flow.t Or_error.t)
      ~(this_stm :
         target:Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(if_stm rest) target
    | In_flow rest ->
        handle_in_flow ~f:(flow rest) target
    | This_stm ->
        this_stm ~target

  let check_path (path : Path.Stm.t) ~(filter : Path_filter.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If.check_path ~filter)
      ~flow:(Flow.check_path ~filter) ~this_stm:(fun ~target ->
        Tx.Or_error.tee_m target ~f:(fun stm ->
            Path_filter.check_final_statement filter ~stm ) )

  let insert_stm (path : Path.Stm.t) ~(to_insert : Subject.Statement.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If.insert_stm ~to_insert)
      ~flow:(Flow.insert_stm ~to_insert) ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message "Can't insert statement here" ~path:(path : Path.Stm.t)] )

  let insert_stm_list (path : Path.Stm.t)
      ~(to_insert : Subject.Statement.t list) ~(target : Subject.Statement.t)
      : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If.insert_stm_list ~to_insert)
      ~flow:(Flow.insert_stm_list ~to_insert) ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message "Can't insert statements here" ~path:(path : Path.Stm.t)] )

  let transform_stm (path : Path.Stm.t)
      ~(f : Subject.Statement.t transformer) ~(target : Subject.Statement.t)
      : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If.transform_stm ~f)
      ~flow:(Flow.transform_stm ~f) ~this_stm:(fun ~target -> f target)

  let transform_stm_list (path : Path.Stm.t)
      ~(f : Subject.Statement.t list transformer)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If.transform_stm_list ~f)
      ~flow:(Flow.transform_stm_list ~f) ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message
            "Can't transform multiple statements here"
              ~path:(path : Path.Stm.t)] )
end

and Block :
  (Path_types.S_consumer
    with type t = Path.Stms.t
     and type target = Subject.Block.t) = struct
  type t = Path.Stms.t

  type target = Subject.Block.t

  module Block_stms = Fir.Block.On_meta_statement_list (Fir.Statement)
  module Block_stms_err = Block_stms.On_monad (Or_error)

  let handle_in_stm (dest : target) (index : int)
      ~(f : target:Subject.Statement.t -> Subject.Statement.t Or_error.t) :
      target Or_error.t =
    Block_stms_err.map_m dest ~f:(fun stms ->
        Tx.List.With_errors.replace_m stms index ~f:(fun target ->
            Or_error.(f ~target >>| Option.some) ) )

  let bad_stm_list_path_error (path : Path.Stms.t)
      ~(here : Source_code_position.t) ~(context : string) :
      target Or_error.t =
    Or_error.error_s
      [%message
        "Can't use this statement-list path here"
          ~here:(here : Source_code_position.t)
          ~context
          ~path:(path : Path.Stms.t)]

  let check_path (path : Path.Stms.t) ~(filter : Path_filter.t)
      ~(target : target) : target Or_error.t =
    let filter =
      Path_filter.update_with_block_metadata filter
        (Fir.Block.metadata target)
    in
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.check_path rest ~filter)
    | Insert _ | On_range (_, _) ->
        Tx.Or_error.(Path_filter.check filter >> Or_error.return target)

  let insert_stm (path : Path.Stms.t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    match path with
    | Insert index ->
        Block_stms_err.map_m target ~f:(fun stms ->
            Tx.List.insert stms index to_insert )
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.insert_stm rest ~to_insert)
    | On_range (_, _) ->
        bad_stm_list_path_error path ~context:"insert_stm" ~here:[%here]

  let insert_stm_list (path : Path.Stms.t)
      ~(to_insert : Subject.Statement.t list) ~(target : target) :
      target Or_error.t =
    (* TODO(@MattWindsor91): implement this more efficiently. *)
    Tx.List.With_errors.fold_m (List.rev to_insert) ~init:target
      ~f:(fun target to_insert -> insert_stm path ~to_insert ~target)

  let transform_stm (path : Path.Stms.t)
      ~(f : Subject.Statement.t transformer) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.transform_stm rest ~f)
    | On_range (pos, len) ->
        Block_stms_err.map_m target
          ~f:(Act_utils.My_list.try_map_sub ~pos ~len ~f)
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm" ~here:[%here]

  let transform_stm_list (path : Path.Stms.t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.transform_stm_list rest ~f)
    | On_range (pos, len) ->
        Block_stms_err.map_m target
          ~f:(Act_utils.My_list.try_splice ~pos ~len ~replace_f:f)
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm_list"
          ~here:[%here]
end

and If :
  (Path_types.S_consumer
    with type t = Path.If.t
     and type target = Subject.Statement.If.t) = struct
  type t = Path.If.t

  type target = Subject.Statement.If.t

  module B = Fir.If.Base_map (Or_error)

  let handle_stm (path : t)
      ~(f :
         Path.Stms.t -> target:Subject.Block.t -> Subject.Block.t Or_error.t)
      ~(target : target) : target Or_error.t =
    let lift_f rest target = f rest ~target in
    match path with
    | In_block (branch, rest) ->
        let t_branch = if branch then lift_f rest else Or_error.return in
        let f_branch = if branch then Or_error.return else lift_f rest in
        B.bmap target ~cond:Or_error.return ~t_branch ~f_branch
    | This_cond ->
        Or_error.error_string "Not a statement path"

  let check_path (path : t) ~(filter : Path_filter.t) ~(target : target) :
      target Or_error.t =
    (* TODO(@MattWindsor91): note in filter that we're in an if statement *)
    handle_stm path ~target ~f:(Block.check_path ~filter)

  let insert_stm_list (path : t) ~(to_insert : Subject.Statement.t list)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm_list ~to_insert)

  let insert_stm (path : t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm ~to_insert)

  let transform_stm (path : t) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm ~f)

  let transform_stm_list (path : t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm_list ~f)
end

and Flow :
  (Path_types.S_consumer
    with type t = Path.Flow.t
     and type target = Subject.Statement.Flow.t) = struct
  type t = Path.Flow.t

  type target = Subject.Statement.Flow.t

  module B = Fir.Flow_block.Base_map (Or_error)

  let handle_stm (path : t)
      ~(f :
         Path.Stms.t -> target:Subject.Block.t -> Subject.Block.t Or_error.t)
      ~(target : target) : target Or_error.t =
    match path with
    | In_block rest ->
        let body target = f rest ~target in
        B.bmap target ~body ~header:Or_error.return
    | This_cond ->
        Or_error.error_string "Not a statement path"

  let check_path (path : t) ~(filter : Path_filter.t) ~(target : target) :
      target Or_error.t =
    let flow = Act_fir.Statement_class.Flow.classify target in
    let filter = Path_filter.update_with_flow ?flow filter in
    handle_stm path ~target ~f:(Block.check_path ~filter)

  let insert_stm_list (path : t) ~(to_insert : Subject.Statement.t list)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm_list ~to_insert)

  let insert_stm (path : t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm ~to_insert)

  let transform_stm (path : t) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm ~f)

  let transform_stm_list (path : Path.Flow.t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm_list ~f)
end

module Thread :
  Path_types.S_consumer
    with type t = Path.Thread.t
     and type target = Subject.Thread.t = struct
  type t = Path.Thread.t

  type target = Subject.Thread.t

  let handle_stm (path : t)
      ~(f :
         Path.Stms.t -> target:Subject.Block.t -> Subject.Block.t Or_error.t)
      ~(target : target) : target Or_error.t =
    match path with
    | In_stms rest ->
        (* TODO(@MattWindsor91): fix this horrific mess *)
        Or_error.(
          f rest
            ~target:(Subject.Block.make_existing ~statements:target.stms ())
          >>| fun block -> {target with stms= Fir.Block.statements block})

  let check_path (path : t) ~(filter : Path_filter.t) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Block.check_path ~filter)

  let insert_stm_list (path : t) ~(to_insert : Subject.Statement.t list)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm_list ~to_insert)

  let insert_stm (path : t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.insert_stm ~to_insert)

  let transform_stm (path : t) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm ~f)

  let transform_stm_list (path : t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Block.transform_stm_list ~f)
end

module Test :
  Path_types.S_consumer
    with type t = Path.Program.t
     and type target = Subject.Test.t = struct
  type t = Path.Program.t

  type target = Subject.Test.t

  let handle_stm (path : t)
      ~(f :
            Path.Thread.t
         -> target:Subject.Thread.t
         -> Subject.Thread.t Or_error.t) ~(target : target) :
      target Or_error.t =
    match path with
    | In_thread (index, rest) ->
        Act_litmus.Test.Raw.try_map_thread ~index
          ~f:(fun target -> f rest ~target)
          target

  let check_path (path : t) ~(filter : Path_filter.t) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Thread.check_path ~filter)

  let insert_stm_list (path : t) ~(to_insert : Subject.Statement.t list)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Thread.insert_stm_list ~to_insert)

  let insert_stm (path : t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Thread.insert_stm ~to_insert)

  let transform_stm (path : t) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Thread.transform_stm ~f)

  let transform_stm_list (path : t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Thread.transform_stm_list ~f)
end
