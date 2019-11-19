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

(* Helpers for making path generators. *)

let map_opt_gen (type a b) (gen : a Q.Generator.t option) ~(f : a -> b) :
    b Q.Generator.t option =
  Option.map ~f:(Q.Generator.map ~f) gen

let union_opt (type a) (gens : a Q.Generator.t option list) :
    a Q.Generator.t option =
  Act_utils.My_list.guard_if_empty ~f:Q.Generator.union
    (List.filter_opt gens)

module rec Statement :
  (Path_types.S_producer
    with type t = Path.stm
     and type target = Subject.Statement.t) = struct
  type t = Path.stm

  type target = Subject.Statement.t

  (* TODO(@MattWindsor91): Travesty? *)
  let nope (type a b) (_ : a) : b option = None

  let try_gen_recursively (m : Subject.Statement.t)
      ~(if_stm : If_statement.target -> Path.ifs Q.Generator.t option) :
      Path.stm Q.Generator.t option =
    Stm.reduce m
      ~if_stm:(fun x -> map_opt_gen ~f:Path.in_if (if_stm x))
      ~while_loop:nope (* for now *) ~assign:nope ~atomic_cmpxchg:nope
      ~atomic_store:nope ~nop:nope

  let try_gen_insert_stm ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.stm Q.Generator.t option =
    try_gen_recursively m ~if_stm:(If_statement.try_gen_insert_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.stm Q.Generator.t option =
    try_gen_recursively m
      ~if_stm:(If_statement.try_gen_transform_stm_list ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t = Path_filter.empty)
      (stm : Subject.Statement.t) : Path.stm Q.Generator.t option =
    let gen_base =
      Option.some_if
        (Path_filter.is_final_statement_ok filter ~stm)
        (Q.Generator.return Path.this_stm)
    in
    let gen_rec =
      try_gen_recursively stm
        ~if_stm:(If_statement.try_gen_transform_stm ~filter)
    in
    union_opt [gen_base; gen_rec]
end

and Statement_list :
  (Path_types.S_producer
    with type t = Path.stm_list
     and type target = Subject.Statement.t list) = struct
  type t = Path.stm_list

  type target = Subject.Statement.t list

  let gen_insert_stm_on ?(filter : Path_filter.t option) (index : int)
      (single_dest : Subject.Statement.t) : t Q.Generator.t list =
    let insert_after = Q.Generator.return (Path.insert (index + 1)) in
    let insert_into =
      single_dest
      |> Statement.try_gen_insert_stm ?filter
      |> map_opt_gen ~f:(Path.in_stm index)
      |> Option.to_list
    in
    insert_after :: insert_into

  let try_gen_insert_stm ?(filter : Path_filter.t option) (dest : target) :
      Path.stm_list Q.Generator.t option =
    Some
      (Q.Generator.union
         ( Q.Generator.return (Path.insert 0)
         :: List.concat_mapi ~f:(gen_insert_stm_on ?filter) dest ))

  let gen_transform_stm_on ?(filter : Path_filter.t option) (index : int)
      (single_dest : Subject.Statement.t) : t Q.Generator.t option =
    single_dest
    |> Statement.try_gen_transform_stm ?filter
    |> map_opt_gen ~f:(Path.in_stm index)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.stm_list Q.Generator.t option =
    Act_utils.My_list.guard_if_empty_opt ~f:(fun dest ->
        union_opt (List.mapi ~f:(gen_transform_stm_on ?filter) dest))

  let gen_transform_stm_list_here (dest : target) : t Q.Generator.t =
    Q.Generator.(
      create (fun ~size ~random ->
          ignore size ;
          Option.value_exn (Act_utils.My_list.Random.stride dest ~random))
      >>| fun (p, d) -> Path.On_stm_range (p, d))

  let gen_transform_stm_list_on ?(filter : Path_filter.t option)
      (index : int) (single_dest : Subject.Statement.t) :
      t Q.Generator.t list =
    single_dest
    |> Statement.try_gen_transform_stm_list ?filter
    |> map_opt_gen ~f:(Path.in_stm index)
    |> Option.to_list

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> t Q.Generator.t option =
    Act_utils.My_list.guard_if_empty ~f:(fun dest ->
        Q.Generator.union
          ( gen_transform_stm_list_here dest
          :: List.concat_mapi dest ~f:(gen_transform_stm_list_on ?filter) ))
end

and If_statement :
  (Path_types.S_producer
    with type t = Path.ifs
     and type target = Subject.Statement.If.t) = struct
  type t = Path.ifs

  type target = Subject.Statement.If.t

  let gen_opt_over_block (branch : bool) (block : Subject.Block.t)
      ~(f : Subject.Statement.t list -> Path.stm_list Q.Generator.t option) :
      Path.ifs Q.Generator.t option =
    map_opt_gen
      (f (Act_c_mini.Block.statements block))
      ~f:(Path.in_block branch)

  let gen_opt_over_blocks (ifs : Metadata.t Stm.If.t)
      ~(f : Subject.Statement.t list -> Path.stm_list Q.Generator.t option) :
      Path.ifs Q.Generator.t option =
    union_opt
      [ gen_opt_over_block ~f true (Stm.If.t_branch ifs)
      ; gen_opt_over_block ~f false (Stm.If.f_branch ifs) ]

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      Metadata.t Stm.If.t -> Path.ifs Q.Generator.t option =
    gen_opt_over_blocks ~f:(Statement_list.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      Metadata.t Stm.If.t -> Path.ifs Q.Generator.t option =
    gen_opt_over_blocks ~f:(Statement_list.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      Metadata.t Stm.If.t -> Path.ifs Q.Generator.t option =
    gen_opt_over_blocks
      ~f:(Statement_list.try_gen_transform_stm_list ?filter)
end

module Thread :
  Path_types.S_producer
    with type t = Path.func
     and type target = Subject.Thread.t = struct
  type t = Path.func

  type target = Subject.Thread.t

  let gen_opt_over_stms ({stms; _} : target)
      ~(f : Subject.Statement.t list -> Path.stm_list Q.Generator.t option) :
      Path.func Q.Generator.t option =
    map_opt_gen (f stms) ~f:Path.in_stms

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.func Q.Generator.t option =
    gen_opt_over_stms ~f:(Statement_list.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.func Q.Generator.t option =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.func Q.Generator.t option =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm_list ?filter)
end

module Test :
  Path_types.S_producer
    with type t = Path.program
     and type target = Subject.Test.t = struct
  type t = Path.program

  type target = Subject.Test.t

  let map_threads (test : target) ~(f : int -> Subject.Thread.t -> 'a) :
      'a list =
    List.mapi (Act_litmus.Test.Raw.threads test) ~f

  let gen_opt_over_threads (test : target)
      ~(f : Subject.Thread.t -> Path.func Q.Generator.t option) :
      Path.program Q.Generator.t option =
    test
    |> map_threads ~f:(fun index prog ->
           map_opt_gen (f prog) ~f:(Path.in_func index))
    |> union_opt

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.program Q.Generator.t option =
    gen_opt_over_threads ~f:(Thread.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.program Q.Generator.t option =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.program Q.Generator.t option =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm_list ?filter)
end
