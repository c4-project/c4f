(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Stm = Act_c_mini.Statement

type 'a transformer = 'a -> 'a Or_error.t

module rec Statement :
  (Path_types.S_consumer
    with type t = Path.stm
     and type target = Subject.Statement.t) = struct
  type t = Path.stm

  type target = Subject.Statement.t

  let in_if_error (dest : Subject.Statement.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here]
          ~target:(Stm.erase_meta dest : unit Stm.t)]

  let handle_in_if (dest : Subject.Statement.t)
      ~(f :
         target:Subject.Statement.If.t -> Subject.Statement.If.t Or_error.t)
      : Subject.Statement.t Or_error.t =
    Stm.reduce dest
      ~if_stm:(fun target -> Or_error.(f ~target >>| Stm.if_stm))
      ~while_loop:(in_if_error dest) ~prim:(in_if_error dest)

  let handle_path (path : Path.stm)
      ~(if_stm :
            Path.ifs
         -> target:Subject.Statement.If.t
         -> Subject.Statement.If.t Or_error.t)
      ~(this_stm :
         target:Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(if_stm rest) target
    | This_stm ->
        this_stm ~target

  let insert_stm (path : Path.stm) ~(to_insert : Subject.Statement.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.insert_stm ~to_insert)
      ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message "Can't insert statement here" ~path:(path : Path.stm)])

  let transform_stm (path : Path.stm) ~(f : Subject.Statement.t transformer)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.transform_stm ~f)
      ~this_stm:(fun ~target -> f target)

  let transform_stm_list (path : Path.stm)
      ~(f : Subject.Statement.t list transformer)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.transform_stm_list ~f)
      ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message
            "Can't transform multiple statements here" ~path:(path : Path.stm)])
end

and Statement_list :
  (Path_types.S_consumer
    with type t = Path.stm_list
     and type target = Subject.Statement.t list) = struct
  type t = Path.stm_list

  type target = Subject.Statement.t list

  let handle_in_stm (dest : target) (index : int)
      ~(f : target:Subject.Statement.t -> Subject.Statement.t Or_error.t) :
      target Or_error.t =
    Tx.List.With_errors.replace_m dest index ~f:(fun target ->
        Or_error.(f ~target >>| Option.some))

  let bad_stm_list_path_error (path : Path.stm_list)
      ~(here : Source_code_position.t) ~(context : string) :
      target Or_error.t =
    Or_error.error_s
      [%message
        "Can't use this statement-list path here"
          ~here:(here : Source_code_position.t)
          ~context
          ~path:(path : Path.stm_list)]

  let insert_stm (path : Path.stm_list) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    match path with
    | Insert index ->
        Tx.List.insert target index to_insert
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.insert_stm rest ~to_insert)
    | On_stm_range (_, _) ->
        bad_stm_list_path_error path ~context:"insert_stm" ~here:[%here]

  let transform_stm (path : Path.stm_list)
      ~(f : Subject.Statement.t transformer) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.transform_stm rest ~f)
    | On_stm_range (pos, len) ->
        Act_utils.My_list.try_map_sub ~pos ~len ~f target
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm" ~here:[%here]

  let transform_stm_list (path : Path.stm_list)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(Statement.transform_stm_list rest ~f)
    | On_stm_range (pos, len) ->
        Act_utils.My_list.try_splice ~pos ~len ~replace_f:f target
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm_list"
          ~here:[%here]
end

and If_statement :
  (Path_types.S_consumer
    with type t = Path.ifs
     and type target = Subject.Statement.If.t) = struct
  type t = Path.ifs

  type target = Subject.Statement.If.t

  module B = Stm.If.Base_map (Or_error)
  module Block_stms = Act_c_mini.Block.On_meta_statement_list (Stm)
  module Block_stms_err = Block_stms.On_monad (Or_error)

  (* TODO(@MattWindsor91): this will probably need to be changed to accept
     full blocks. *)

  let handle_stm (path : Path.ifs)
      ~(f :
            Path.stm_list
         -> target:Subject.Statement.t list
         -> Subject.Statement.t list Or_error.t) ~(target : target) :
      target Or_error.t =
    let lift_f rest =
      Block_stms_err.map_m ~f:(fun target -> f rest ~target)
    in
    match path with
    | In_block (branch, rest) ->
        let t_branch, f_branch =
          ( (if branch then lift_f rest else Or_error.return)
          , if branch then Or_error.return else lift_f rest )
        in
        B.bmap target ~cond:Or_error.return ~t_branch ~f_branch
    | This_cond ->
        Or_error.error_string "Not a statement path"

  let insert_stm (path : Path.ifs) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.insert_stm ~to_insert)

  let transform_stm (path : Path.ifs) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm ~f)

  let transform_stm_list (path : Path.ifs)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm_list ~f)
end

module Thread :
  Path_types.S_consumer
    with type t = Path.func
     and type target = Subject.Thread.t = struct
  type t = Path.func

  type target = Subject.Thread.t

  let handle_stm (path : t)
      ~(f :
            Path.stm_list
         -> target:Subject.Statement.t list
         -> Subject.Statement.t list Or_error.t) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stms rest ->
        Or_error.(
          f rest ~target:target.stms
          >>| fun stms' -> {target with stms= stms'})

  let insert_stm (path : t) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(fun rest ->
        Statement_list.insert_stm rest ~to_insert)

  let transform_stm (path : t) ~(f : Subject.Statement.t transformer)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm ~f)

  let transform_stm_list (path : t)
      ~(f : Subject.Statement.t list transformer) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm_list ~f)
end

module Test :
  Path_types.S_consumer
    with type t = Path.program
     and type target = Subject.Test.t = struct
  type t = Path.program

  type target = Subject.Test.t

  let handle_stm (path : t)
      ~(f :
         Path.func -> target:Subject.Thread.t -> Subject.Thread.t Or_error.t)
      ~(target : target) : target Or_error.t =
    match path with
    | In_func (index, rest) ->
        Act_litmus.Test.Raw.try_map_thread ~index
          ~f:(fun target -> f rest ~target)
          target

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
