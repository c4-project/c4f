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

let check_ok (type a) (x : a) ~(filter : Path_filter.t) : a option =
  Option.some_if (Path_filter.is_ok filter) x

module rec Statement :
  (Path_types.S_producer
    with type t = Path.stm
     and type target = Subject.Statement.t) = struct
  type t = Path.stm

  type target = Subject.Statement.t

  (* TODO(@MattWindsor91): Travesty? *)
  let nope (type a b) (_ : a) : b option = None

  let try_gen_recursively (m : Subject.Statement.t)
      ~(if_stm : If_statement.target -> Path.ifs Opt_gen.t) :
      Path.stm Opt_gen.t =
    Stm.reduce m
      ~if_stm:(fun x -> Opt_gen.map ~f:Path.in_if (if_stm x))
      ~while_loop:nope (* for now *) ~prim:nope

  let try_gen_insert_stm ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.stm Opt_gen.t =
    try_gen_recursively m ~if_stm:(If_statement.try_gen_insert_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.stm Opt_gen.t =
    try_gen_recursively m
      ~if_stm:(If_statement.try_gen_transform_stm_list ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t = Path_filter.empty)
      (stm : Subject.Statement.t) : Path.stm Opt_gen.t =
    let gen_base =
      Option.some_if
        (Path_filter.is_final_statement_ok filter ~stm)
        (Q.Generator.return Path.this_stm)
    in
    let gen_rec =
      try_gen_recursively stm
        ~if_stm:(If_statement.try_gen_transform_stm ~filter)
    in
    Opt_gen.union [gen_base; gen_rec]
end

and Statement_list :
  (Path_types.S_producer
    with type t = Path.stm_list
     and type target = Subject.Statement.t list) = struct
  type t = Path.stm_list

  type target = Subject.Statement.t list

  module Insert (I : sig
    val filter : Path_filter.t
  end) =
  struct
    let filter = I.filter

    let try_gen_here (index : int) : Path.stm_list Opt_gen.t =
      check_ok ~filter (Q.Generator.return (Path.insert index))

    let try_gen_inside (index : int) (single_dest : Subject.Statement.t) :
        Path.stm_list Opt_gen.t =
      single_dest
      |> Statement.try_gen_insert_stm ~filter
      |> Opt_gen.map ~f:(Path.in_stm index)

    let try_gen_at (index : int) (single_dest : Subject.Statement.t) :
        t Opt_gen.t list =
      let insert_after = try_gen_here (index + 1) in
      let insert_inside = try_gen_inside index single_dest in
      [insert_after; insert_inside]

    let try_gen_at_each : target -> t Opt_gen.t list =
      List.concat_mapi ~f:try_gen_at
  end

  let try_gen_insert_stm ?(filter : Path_filter.t = Path_filter.empty)
      (dest : target) : Path.stm_list Opt_gen.t =
    let module I = Insert (struct
      let filter = filter
    end) in
    Opt_gen.union (I.try_gen_here 0 :: I.try_gen_at_each dest)

  let gen_transform_stm_on ?(filter : Path_filter.t option) (index : int)
      (single_dest : Subject.Statement.t) : t Opt_gen.t =
    single_dest
    |> Statement.try_gen_transform_stm ?filter
    |> Opt_gen.map ~f:(Path.in_stm index)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.stm_list Opt_gen.t =
    Act_utils.My_list.guard_if_empty_opt ~f:(fun dest ->
        Opt_gen.union (List.mapi ~f:(gen_transform_stm_on ?filter) dest))

  let gen_transform_stm_list_here (dest : target) : t Q.Generator.t =
    Q.Generator.(
      create (fun ~size ~random ->
          ignore size ;
          Option.value_exn (Act_utils.My_list.Random.stride dest ~random))
      >>| fun (p, d) -> Path.On_stm_range (p, d))

  let gen_transform_stm_list_on (index : int)
      (single_dest : Subject.Statement.t) ~(filter : Path_filter.t) :
      t Opt_gen.t =
    let f = filter in
    Option.(
      single_dest
      |> Statement.try_gen_transform_stm_list ~filter:f
      |> Opt_gen.map ~f:(Path.in_stm index)
      >>= check_ok ~filter:f)

  let try_gen_transform_stm_list
      ?(filter : Path_filter.t = Path_filter.empty) : target -> t Opt_gen.t =
    Act_utils.My_list.guard_if_empty_opt ~f:(fun dest ->
        Opt_gen.union
          ( check_ok ~filter (gen_transform_stm_list_here dest)
          :: List.mapi dest ~f:(gen_transform_stm_list_on ~filter) ))
end

and If_statement :
  (Path_types.S_producer
    with type t = Path.ifs
     and type target = Subject.Statement.If.t) = struct
  type t = Path.ifs

  type target = Subject.Statement.If.t

  let gen_opt_over_block ?(filter : Path_filter.t = Path_filter.empty)
      (branch : bool) (block : Subject.Block.t)
      ~(f :
            ?filter:Path_filter.t
         -> Subject.Statement.t list
         -> Path.stm_list Opt_gen.t) : Path.ifs Opt_gen.t =
    let filter' =
      Path_filter.update_with_block_metadata filter
        (Act_c_mini.Block.metadata block)
    in
    Opt_gen.map
      (f ~filter:filter' (Act_c_mini.Block.statements block))
      ~f:(Path.in_block branch)

  let gen_opt_over_blocks ?(filter : Path_filter.t option)
      (ifs : Metadata.t Stm.If.t)
      ~(f :
            ?filter:Path_filter.t
         -> Subject.Statement.t list
         -> Path.stm_list Opt_gen.t) : Path.ifs Opt_gen.t =
    Opt_gen.union
      [ gen_opt_over_block ?filter ~f true (Stm.If.t_branch ifs)
      ; gen_opt_over_block ?filter ~f false (Stm.If.f_branch ifs) ]

  let try_gen_insert_stm :
      ?filter:Path_filter.t -> Metadata.t Stm.If.t -> Path.ifs Opt_gen.t =
    gen_opt_over_blocks ~f:Statement_list.try_gen_insert_stm

  let try_gen_transform_stm :
      ?filter:Path_filter.t -> Metadata.t Stm.If.t -> Path.ifs Opt_gen.t =
    gen_opt_over_blocks ~f:Statement_list.try_gen_transform_stm

  let try_gen_transform_stm_list :
      ?filter:Path_filter.t -> Metadata.t Stm.If.t -> Path.ifs Opt_gen.t =
    gen_opt_over_blocks ~f:Statement_list.try_gen_transform_stm_list
end

module Thread :
  Path_types.S_producer
    with type t = Path.func
     and type target = Subject.Thread.t = struct
  type t = Path.func

  type target = Subject.Thread.t

  let gen_opt_over_stms ({stms; _} : target)
      ~(f : Subject.Statement.t list -> Path.stm_list Opt_gen.t) :
      Path.func Opt_gen.t =
    Opt_gen.map (f stms) ~f:Path.in_stms

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.func Opt_gen.t =
    gen_opt_over_stms ~f:(Statement_list.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.func Opt_gen.t =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.func Opt_gen.t =
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
      ~(f : Subject.Thread.t -> Path.func Opt_gen.t) : Path.program Opt_gen.t
      =
    test
    |> map_threads ~f:(fun index prog ->
           Opt_gen.map (f prog) ~f:(Path.in_func index))
    |> Opt_gen.union

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.program Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.program Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.program Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm_list ?filter)
end
