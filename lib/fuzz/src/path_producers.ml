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

let check_ok (type a) (x : a) ~(filter : Path_filter.t) : a Or_error.t =
  if Path_filter.is_ok filter then Or_error.return x
  else Or_error.error_string "This path failed a filter check"

let lift_over_block ?(filter : Path_filter.t = Path_filter.empty)
    (block : Subject.Block.t)
    ~(f :
          ?filter:Path_filter.t
       -> Subject.Statement.t list
       -> Path.stm_list Opt_gen.t) : Path.stm_list Opt_gen.t =
  let filter' =
    Path_filter.update_with_block_metadata filter
      (Act_c_mini.Block.metadata block)
  in
  f ~filter:filter' (Act_c_mini.Block.statements block)

module rec Statement :
  (Path_types.S_producer
    with type t = Path.stm
     and type target = Subject.Statement.t) = struct
  type t = Path.stm

  type target = Subject.Statement.t

  let not_in_prim (type a b) (_ : a) ~(kind : string) : b Or_error.t =
    Or_error.error_s
      [%message
        "Can't generate a path of this type on a primitive statement" ~kind]

  let try_gen_recursively (m : Subject.Statement.t) ~(kind : string)
      ~(if_stm : If.target -> Path.If.t Opt_gen.t)
      ~(while_loop : Loop.target -> Path.Loop.t Opt_gen.t) :
      Path.Stm.t Opt_gen.t =
    Opt_gen.(
      Stm.reduce m
        ~if_stm:(fun x -> x |> if_stm >>| Path.Stm.in_if)
        ~while_loop:(fun x -> x |> while_loop >>| Path.Stm.in_loop)
        ~prim:(not_in_prim ~kind))

  let try_gen_insert_stm ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    try_gen_recursively m ~kind:"insert_stm"
      ~if_stm:(If.try_gen_insert_stm ?filter)
      ~while_loop:(Loop.try_gen_insert_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    try_gen_recursively m ~kind:"transform_stm_list"
      ~if_stm:(If.try_gen_transform_stm_list ?filter)
      ~while_loop:(Loop.try_gen_transform_stm_list ?filter)

  let this_stm_if_ok (stm : Subject.Statement.t) ~(filter : Path_filter.t) :
      Path.Stm.t Opt_gen.t =
    if Path_filter.is_final_statement_ok filter ~stm then
      Opt_gen.return Path.Stm.this_stm
    else
      Or_error.error_s
        [%message
          "Generated 'this-statement' path failed filtering checks"
            ~stm:(stm : Subject.Statement.t)]

  let try_gen_transform_stm ?(filter : Path_filter.t = Path_filter.empty)
      (stm : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    let gen_base = this_stm_if_ok stm ~filter in
    let gen_rec =
      try_gen_recursively stm ~kind:"transform_stm"
        ~if_stm:(If.try_gen_transform_stm ~filter)
        ~while_loop:(Loop.try_gen_transform_stm ~filter)
    in
    Opt_gen.union [gen_base; gen_rec]
end

and Statement_list :
  (Path_types.S_producer
    with type t = Path.stm_list
     and type target = Subject.Statement.t list) = struct
  type t = Path.stm_list

  let in_stm (index : int) : Path.Stm.t Opt_gen.t -> Path.Stms.t Opt_gen.t =
    Opt_gen.map ~f:(Path.Stms.in_stm index)

  type target = Subject.Statement.t list

  module Insert (I : sig
    val filter : Path_filter.t
  end) =
  struct
    let filter = I.filter

    let try_gen_here (index : int) : Path.stm_list Opt_gen.t =
      check_ok ~filter (Q.Generator.return (Path.Stms.insert index))

    let try_gen_inside (index : int) (single_dest : Subject.Statement.t) :
        Path.stm_list Opt_gen.t =
      single_dest |> Statement.try_gen_insert_stm ~filter |> in_stm index

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
    single_dest |> Statement.try_gen_transform_stm ?filter |> in_stm index

  let try_gen_transform_stm ?(filter : Path_filter.t option) (dest : target)
      : Path.stm_list Opt_gen.t =
    Opt_gen.union (List.mapi ~f:(gen_transform_stm_on ?filter) dest)

  let gen_transform_stm_list_here_populated (dest : target) : t Q.Generator.t
      =
    Q.Generator.(
      create (fun ~size ~random ->
          ignore size ;
          Option.value_exn (Act_utils.My_list.Random.stride dest ~random))
      >>| fun (p, d) -> Path.Stms.on_range p d)

  let gen_transform_stm_list_here (dest : target) : t Q.Generator.t =
    let len = List.length dest in
    let after = Q.Generator.return (Path.Stms.on_range len 0) in
    let inner =
      if len = 0 then [] else [gen_transform_stm_list_here_populated dest]
    in
    Q.Generator.union (after :: inner)

  let gen_transform_stm_list_on (index : int)
      (single_dest : Subject.Statement.t) ~(filter : Path_filter.t) :
      t Opt_gen.t =
    let f = filter in
    Or_error.(
      single_dest
      |> Statement.try_gen_transform_stm_list ~filter:f
      |> in_stm index >>= check_ok ~filter:f)

  let try_gen_transform_stm_list
      ?(filter : Path_filter.t = Path_filter.empty) (dest : target) :
      t Opt_gen.t =
    Opt_gen.union
      ( check_ok ~filter (gen_transform_stm_list_here dest)
      :: List.mapi dest ~f:(gen_transform_stm_list_on ~filter) )
end

and If :
  (Path_types.S_producer
    with type t = Path.If.t
     and type target = Subject.Statement.If.t) = struct
  type t = Path.If.t

  type target = Subject.Statement.If.t

  let gen_opt_over_block ?(filter : Path_filter.t = Path_filter.empty)
      (branch : bool) (block : Subject.Block.t)
      ~(f :
            ?filter:Path_filter.t
         -> Subject.Statement.t list
         -> Path.Stms.t Opt_gen.t) : Path.If.t Opt_gen.t =
    Opt_gen.map
      (lift_over_block ~f ~filter block)
      ~f:(Path.If.in_branch branch)

  let gen_opt_over_blocks ?(filter : Path_filter.t option)
      (ifs : Metadata.t Stm.If.t)
      ~(f :
            ?filter:Path_filter.t
         -> Subject.Statement.t list
         -> Path.stm_list Opt_gen.t) : Path.If.t Opt_gen.t =
    Opt_gen.union
      [ gen_opt_over_block ?filter ~f true (Stm.If.t_branch ifs)
      ; gen_opt_over_block ?filter ~f false (Stm.If.f_branch ifs) ]

  let try_gen_insert_stm :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Statement_list.try_gen_insert_stm

  let try_gen_transform_stm :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Statement_list.try_gen_transform_stm

  let try_gen_transform_stm_list :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Statement_list.try_gen_transform_stm_list
end

and Loop :
  (Path_types.S_producer
    with type t = Path.Loop.t
     and type target = Subject.Statement.Loop.t) = struct
  type t = Path.Loop.t

  type target = Subject.Statement.Loop.t

  let gen_opt_over_body ?(filter : Path_filter.t = Path_filter.empty)
      (loop : Subject.Statement.Loop.t)
      ~(f :
            ?filter:Path_filter.t
         -> Subject.Statement.t list
         -> Path.Stms.t Opt_gen.t) : Path.Loop.t Opt_gen.t =
    Opt_gen.map
      (lift_over_block ~f ~filter (Stm.While.body loop))
      ~f:Path.Loop.in_body

  let try_gen_insert_stm :
         ?filter:Path_filter.t
      -> Subject.Statement.Loop.t
      -> Path.Loop.t Opt_gen.t =
    gen_opt_over_body ~f:Statement_list.try_gen_insert_stm

  let try_gen_transform_stm :
         ?filter:Path_filter.t
      -> Subject.Statement.Loop.t
      -> Path.Loop.t Opt_gen.t =
    gen_opt_over_body ~f:Statement_list.try_gen_transform_stm

  let try_gen_transform_stm_list :
         ?filter:Path_filter.t
      -> Subject.Statement.Loop.t
      -> Path.Loop.t Opt_gen.t =
    gen_opt_over_body ~f:Statement_list.try_gen_transform_stm_list
end

module Thread :
  Path_types.S_producer
    with type t = Path.Thread.t
     and type target = Subject.Thread.t = struct
  type t = Path.Thread.t

  type target = Subject.Thread.t

  let gen_opt_over_stms ({stms; _} : target)
      ~(f : Subject.Statement.t list -> Path.stm_list Opt_gen.t) :
      Path.Thread.t Opt_gen.t =
    Opt_gen.map (f stms) ~f:Path.Thread.in_stms

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Statement_list.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Statement_list.try_gen_transform_stm_list ?filter)
end

module Test :
  Path_types.S_producer
    with type t = Path.Program.t
     and type target = Subject.Test.t = struct
  type t = Path.Program.t

  type target = Subject.Test.t

  let map_threads (test : target) ~(f : int -> Subject.Thread.t -> 'a) :
      'a list =
    List.mapi (Act_litmus.Test.Raw.threads test) ~f

  let gen_opt_over_threads (test : target)
      ~(f : Subject.Thread.t -> Path.Thread.t Opt_gen.t) :
      Path.Program.t Opt_gen.t =
    test
    |> map_threads ~f:(fun index prog ->
           Opt_gen.map (f prog) ~f:(Path.Program.in_thread index))
    |> Opt_gen.union

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.Program.t Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.Program.t Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.Program.t Opt_gen.t =
    gen_opt_over_threads ~f:(Thread.try_gen_transform_stm_list ?filter)
end
