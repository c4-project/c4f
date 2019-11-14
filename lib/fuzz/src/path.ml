(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Q = Base_quickcheck
module Stm = Act_c_mini.Statement
module Fun = Act_c_mini.Function
module Prog = Act_c_mini.Program

let tid : Path_shapes.program -> int = function In_func (t, _) -> t

(* Helpers for making path generators. *)

let map_opt_gen (type a b) (gen : a Q.Generator.t option) ~(f : a -> b) :
    b Q.Generator.t option =
  Option.map ~f:(Q.Generator.map ~f) gen

let union_opt (type a) (gens : a Q.Generator.t option list) :
    a Q.Generator.t option =
  match List.filter_opt gens with
  | [] ->
      None
  | xs ->
      Some (Q.Generator.union xs)

module rec Statement :
  (Path_types.S_statement with type target = Subject.Statement.t) = struct
  type target = Subject.Statement.t

  type t = Path_shapes.stm

  let in_if_error (dest : Subject.Statement.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here]
          ~target:(Stm.erase_meta dest : unit Stm.t)]

  let handle_in_if (dest : Subject.Statement.t)
      ~(f : target:Metadata.t Stm.If.t -> Metadata.t Stm.If.t Or_error.t) :
      Subject.Statement.t Or_error.t =
    Stm.reduce dest
      ~if_stm:(fun target -> Or_error.(f ~target >>| Stm.if_stm))
      ~while_loop:(in_if_error dest) ~assign:(in_if_error dest)
      ~atomic_cmpxchg:(in_if_error dest) ~atomic_store:(in_if_error dest)
      ~nop:(in_if_error dest)

  let handle_path (path : Path_shapes.stm)
      ~(if_stm :
            Path_shapes.ifs
         -> target:Metadata.t Stm.If.t
         -> Metadata.t Stm.If.t Or_error.t)
      ~(this_stm :
         target:Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(if_stm rest) target
    | This_stm ->
        this_stm ~target

  let insert_stm (path : Path_shapes.stm) ~(to_insert : Subject.Statement.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.insert_stm ~to_insert)
      ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message
            "Can't insert statement here" ~path:(path : Path_shapes.stm)])

  let transform_stm (path : Path_shapes.stm)
      ~(f : Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.transform_stm ~f)
      ~this_stm:(fun ~target -> f target)

  let transform_stm_list (path : Path_shapes.stm)
      ~(f : Subject.Statement.t list -> Subject.Statement.t list Or_error.t)
      ~(target : Subject.Statement.t) : Subject.Statement.t Or_error.t =
    handle_path path ~target ~if_stm:(If_statement.transform_stm_list ~f)
      ~this_stm:(fun ~target ->
        ignore target ;
        Or_error.error_s
          [%message
            "Can't transform multiple statements here"
              ~path:(path : Path_shapes.stm)])

  (* TODO(@MattWindsor91): Travesty? *)
  let nope (type a b) (_ : a) : b option = None

  let try_gen_recursively (m : Subject.Statement.t)
      ~(if_stm : If_statement.target -> Path_shapes.ifs Q.Generator.t option)
      : Path_shapes.stm Q.Generator.t option =
    Stm.reduce m
      ~if_stm:(fun x -> map_opt_gen ~f:Path_shapes.in_if (if_stm x))
      ~while_loop:nope (* for now *) ~assign:nope ~atomic_cmpxchg:nope
      ~atomic_store:nope ~nop:nope

  let try_gen_insert_stm (m : Subject.Statement.t) :
      Path_shapes.stm Q.Generator.t option =
    try_gen_recursively m
      ~if_stm:(Fn.compose Option.return If_statement.gen_insert_stm)

  let try_gen_transform_stm_list (m : Subject.Statement.t) :
      Path_shapes.stm Q.Generator.t option =
    try_gen_recursively m ~if_stm:If_statement.try_gen_transform_stm_list

  let try_gen_transform_stm
      ?(predicate : Subject.Statement.t -> bool = Fn.const true)
      (stm : Subject.Statement.t) : Path_shapes.stm Q.Generator.t option =
    let gen_base =
      Option.some_if (predicate stm)
        (Q.Generator.return Path_shapes.this_stm)
    in
    let gen_rec =
      try_gen_recursively stm
        ~if_stm:(If_statement.try_gen_transform_stm ~predicate)
    in
    union_opt [gen_base; gen_rec]
end

and Statement_list :
  (Path_types.S_statement_list with type target = Subject.Statement.t) =
struct
  module M = Statement

  type target = M.target

  type t = Path_shapes.stm_list

  let handle_in_stm (dest : target list) (index : int)
      ~(f : target:target -> target Or_error.t) : target list Or_error.t =
    Tx.List.With_errors.replace_m dest index ~f:(fun target ->
        Or_error.(f ~target >>| Option.some))

  let bad_stm_list_path_error (path : Path_shapes.stm_list)
      ~(here : Source_code_position.t) ~(context : string) :
      target list Or_error.t =
    Or_error.error_s
      [%message
        "Can't use this statement-list path here"
          ~here:(here : Source_code_position.t)
          ~context
          ~path:(path : Path_shapes.stm_list)]

  let insert_stm (path : Path_shapes.stm_list)
      ~(to_insert : Subject.Statement.t) ~(target : target list) :
      target list Or_error.t =
    match path with
    | Insert index ->
        Tx.List.insert target index to_insert
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(M.insert_stm rest ~to_insert)
    | On_stm_range (_, _) ->
        bad_stm_list_path_error path ~context:"insert_stm" ~here:[%here]

  let transform_stm (path : Path_shapes.stm_list)
      ~(f : Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : target list) : target list Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(M.transform_stm rest ~f)
    | On_stm_range (pos, len) ->
        Act_utils.My_list.try_map_sub ~pos ~len ~f target
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm" ~here:[%here]

  let transform_stm_list (path : Path_shapes.stm_list)
      ~(f : Subject.Statement.t list -> Subject.Statement.t list Or_error.t)
      ~(target : target list) : target list Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm target index ~f:(M.transform_stm_list rest ~f)
    | On_stm_range (pos, len) ->
        Act_utils.My_list.try_splice ~pos ~len ~replace_f:f target
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm_list"
          ~here:[%here]

  let gen_insert_stm_on (index : int) (single_dest : target) :
      t Q.Generator.t list =
    let insert_after = Q.Generator.return (Path_shapes.insert (index + 1)) in
    let insert_into =
      single_dest |> M.try_gen_insert_stm
      |> map_opt_gen ~f:(Path_shapes.in_stm index)
      |> Option.to_list
    in
    insert_after :: insert_into

  let gen_insert_stm (dest : target list) :
      Path_shapes.stm_list Q.Generator.t =
    Q.Generator.union
      ( Q.Generator.return (Path_shapes.insert 0)
      :: List.concat_mapi ~f:gen_insert_stm_on dest )

  let gen_transform_stm_on
      ?(predicate : (Subject.Statement.t -> bool) option) (index : int)
      (single_dest : target) : t Q.Generator.t option =
    single_dest
    |> M.try_gen_transform_stm ?predicate
    |> map_opt_gen ~f:(Path_shapes.in_stm index)

  let try_gen_transform_stm
      ?(predicate : (Subject.Statement.t -> bool) option) :
      target list -> Path_shapes.stm_list Q.Generator.t option =
    Act_utils.My_list.guard_if_empty_opt ~f:(fun dest ->
        union_opt (List.mapi ~f:(gen_transform_stm_on ?predicate) dest))

  let gen_transform_stm_list_here (dest : target list) : t Q.Generator.t =
    Q.Generator.(
      create (fun ~size ~random ->
          ignore size ;
          Option.value_exn (Act_utils.My_list.Random.stride dest ~random))
      >>| fun (p, d) -> Path_shapes.On_stm_range (p, d))

  let gen_transform_stm_list_on (index : int) (single_dest : target) :
      t Q.Generator.t list =
    single_dest |> M.try_gen_transform_stm_list
    |> map_opt_gen ~f:(Path_shapes.in_stm index)
    |> Option.to_list

  let try_gen_transform_stm_list : target list -> t Q.Generator.t option =
    Act_utils.My_list.guard_if_empty ~f:(fun dest ->
        Q.Generator.union
          ( gen_transform_stm_list_here dest
          :: List.concat_mapi dest ~f:gen_transform_stm_list_on ))
end

and If_statement :
  (Path_types.S_if_statement with type target = Metadata.t Stm.If.t) = struct
  type target = Metadata.t Stm.If.t

  type stm = Subject.Statement.t

  module B = Stm.If.Base_map (Or_error)
  module Block_stms = Act_c_mini.Block.On_meta_statement_list (Stm)
  module Block_stms_err = Block_stms.On_monad (Or_error)

  (* TODO(@MattWindsor91): this will probably need to be changed to accept
     full blocks. *)

  let handle_stm (path : Path_shapes.ifs)
      ~(f : Path_shapes.stm_list -> target:stm list -> stm list Or_error.t)
      ~(target : target) : target Or_error.t =
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

  let insert_stm (path : Path_shapes.ifs) ~(to_insert : stm)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.insert_stm ~to_insert)

  let transform_stm (path : Path_shapes.ifs) ~(f : stm -> stm Or_error.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm ~f)

  let transform_stm_list (path : Path_shapes.ifs)
      ~(f : stm list -> stm list Or_error.t) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm_list ~f)

  let gen_insert_stm_for_branch (branch : bool)
      (branch_block : Subject.Block.t) : Path_shapes.ifs Q.Generator.t =
    Q.Generator.map
      ~f:(Path_shapes.in_block branch)
      (Statement_list.gen_insert_stm
         (Act_c_mini.Block.statements branch_block))

  let gen_insert_stm (ifs : Metadata.t Stm.If.t) :
      Path_shapes.ifs Q.Generator.t =
    Q.Generator.union
      [ gen_insert_stm_for_branch true (Stm.If.t_branch ifs)
      ; gen_insert_stm_for_branch false (Stm.If.f_branch ifs) ]

  let gen_opt_over_block (branch : bool) (block : Subject.Block.t)
      ~(f :
            Subject.Statement.t list
         -> Path_shapes.stm_list Q.Generator.t option) :
      Path_shapes.ifs Q.Generator.t option =
    map_opt_gen
      (f (Act_c_mini.Block.statements block))
      ~f:(Path_shapes.in_block branch)

  let gen_opt_over_blocks (ifs : Metadata.t Stm.If.t)
      ~(f :
            Subject.Statement.t list
         -> Path_shapes.stm_list Q.Generator.t option) :
      Path_shapes.ifs Q.Generator.t option =
    union_opt
      [ gen_opt_over_block ~f true (Stm.If.t_branch ifs)
      ; gen_opt_over_block ~f false (Stm.If.f_branch ifs) ]

  let try_gen_transform_stm
      ?(predicate : (Subject.Statement.t -> bool) option) :
      Metadata.t Stm.If.t -> Path_shapes.ifs Q.Generator.t option =
    gen_opt_over_blocks ~f:(Statement_list.try_gen_transform_stm ?predicate)

  let try_gen_transform_stm_list :
      Metadata.t Stm.If.t -> Path_shapes.ifs Q.Generator.t option =
    gen_opt_over_blocks ~f:Statement_list.try_gen_transform_stm_list
end

module Thread : Path_types.S_function with type target := Subject.Thread.t =
struct
  type target = Subject.Thread.t

  let handle_stm (path : Path_shapes.func)
      ~(f :
            Path_shapes.stm_list
         -> target:Subject.Statement.t list
         -> Subject.Statement.t list Or_error.t) ~(target : target) :
      target Or_error.t =
    match path with
    | In_stms rest ->
        Or_error.(
          f rest ~target:target.stms
          >>| fun stms' -> {target with stms= stms'})

  let insert_stm (path : Path_shapes.func) ~(to_insert : Subject.Statement.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(fun rest ->
        Statement_list.insert_stm rest ~to_insert)

  let transform_stm (path : Path_shapes.func)
      ~(f : Subject.Statement.t -> Subject.Statement.t Or_error.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm ~f)

  let transform_stm_list (path : Path_shapes.func)
      ~(f : Subject.Statement.t list -> Subject.Statement.t list Or_error.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Statement_list.transform_stm_list ~f)

  let gen_insert_stm ({stms; _} : target) : Path_shapes.func Q.Generator.t =
    Q.Generator.map
      (Statement_list.gen_insert_stm stms)
      ~f:Path_shapes.in_stms

  let gen_opt_over_stms ({stms; _} : target)
      ~(f :
            Subject.Statement.t list
         -> Path_shapes.stm_list Q.Generator.t option) :
      Path_shapes.func Q.Generator.t option =
    map_opt_gen (f stms) ~f:Path_shapes.in_stms

  let try_gen_transform_stm
      ?(predicate : (Subject.Statement.t -> bool) option) :
      target -> Path_shapes.func Q.Generator.t option =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm ?predicate)

  let try_gen_transform_stm_list :
      target -> Path_shapes.func Q.Generator.t option =
    gen_opt_over_stms ~f:Statement_list.try_gen_transform_stm_list
end

module Test : Path_types.S_program with type target := Subject.Test.t =
struct
  type target = Subject.Test.t

  type stm = Subject.Statement.t

  let handle_stm (path : Path_shapes.program)
      ~(f :
            Path_shapes.func
         -> target:Subject.Thread.t
         -> Subject.Thread.t Or_error.t) ~(target : target) :
      target Or_error.t =
    match path with
    | In_func (index, rest) ->
        Act_litmus.Test.Raw.try_map_thread ~index
          ~f:(fun target -> f rest ~target)
          target

  let insert_stm (path : Path_shapes.program) ~(to_insert : stm)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Thread.insert_stm ~to_insert)

  let transform_stm (path : Path_shapes.program) ~(f : stm -> stm Or_error.t)
      ~(target : target) : target Or_error.t =
    handle_stm path ~target ~f:(Thread.transform_stm ~f)

  let transform_stm_list (path : Path_shapes.program)
      ~(f : stm list -> stm list Or_error.t) ~(target : target) :
      target Or_error.t =
    handle_stm path ~target ~f:(Thread.transform_stm_list ~f)

  let map_threads (test : target) ~(f : int -> Subject.Thread.t -> 'a) :
      'a list =
    List.mapi (Act_litmus.Test.Raw.threads test) ~f

  let gen_insert_stm (test : target) : Path_shapes.program Q.Generator.t =
    test
    |> map_threads ~f:(fun index prog ->
           Q.Generator.map
             (Thread.gen_insert_stm prog)
             ~f:(Path_shapes.in_func index))
    |> Q.Generator.union

  let gen_opt_over_threads (test : target)
      ~(f : Subject.Thread.t -> Path_shapes.func Q.Generator.t option) :
      Path_shapes.program Q.Generator.t option =
    test
    |> map_threads ~f:(fun index prog ->
           map_opt_gen (f prog) ~f:(Path_shapes.in_func index))
    |> union_opt

  let try_gen_transform_stm
      ?(predicate : (Subject.Statement.t -> bool) option) :
      target -> Path_shapes.program Q.Generator.t option =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm ?predicate)

  let try_gen_transform_stm_list :
      target -> Path_shapes.program Q.Generator.t option =
    gen_opt_over_threads ~f:Thread.try_gen_transform_stm_list
end
