(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
  module Q = Base_quickcheck
  module Stm = Act_fir.Statement
  module Fun = Act_fir.Function
  module Prog = Act_fir.Program
end

(* Helpers for making path generators. *)

let check_ok (type a) (x : a) ~(filter : Path_filter.t) : a Or_error.t =
  Tx.Or_error.tee_m x ~f:(fun (_ : a) -> Path_filter.check filter)

(* TODO (@MattWindsor91): slated for removal *)
module type S_producer = sig
  type t

  type target

  val try_gen_insert_stm : ?filter:Path_filter.t -> target -> t Opt_gen.t
  (** [try_gen_insert_stm dest] tries to create a Quickcheck-style generator
      for statement insertion paths targeting [dest]. These paths can also be
      used for inserting statement lists.

      It can return an error if [dest] has no position at which statements
      can be inserted. *)

  val try_gen_transform_stm_list :
    ?filter:Path_filter.t -> target -> t Opt_gen.t
  (** [try_gen_transform_stm dest] tries to create a Quickcheck-style
      generator for statement list transformation paths targeting [dest].

      It can return an error if [dest] has no position at which statement
      lists can be transformed. *)

  val try_gen_transform_stm : ?filter:Path_filter.t -> target -> t Opt_gen.t
  (** [try_gen_transform_stm ?predicate dest] tries to create a
      Quickcheck-style generator for statement transformation paths targeting
      [dest], and, optionally, satisfying [filter]. It returns an error if
      the container is empty or no such statements were found. *)
end

module rec Statement :
  (S_producer with type t = Path.stm and type target = Subject.Statement.t) =
struct
  type t = Path.stm

  type target = Subject.Statement.t

  let not_in_prim (type a b) (_ : a) ~(kind : string) : b Or_error.t =
    Or_error.error_s
      [%message
        "Can't generate a path of this type on a primitive statement" ~kind]

  let try_gen_recursively (m : Subject.Statement.t) ~(kind : string)
      ~(if_stm : If.target -> Path.If.t Opt_gen.t)
      ~(flow : Flow.target -> Path.Flow.t Opt_gen.t) : Path.Stm.t Opt_gen.t =
    Opt_gen.(
      Stm.reduce_step m
        ~if_stm:(fun x -> x |> if_stm >>| Path.Stm.in_if)
        ~flow:(fun x -> x |> flow >>| Path.Stm.in_flow)
        ~prim:(not_in_prim ~kind))

  let try_gen_insert_stm ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    try_gen_recursively m ~kind:"insert_stm"
      ~if_stm:(If.try_gen_insert_stm ?filter)
      ~flow:(Flow.try_gen_insert_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option)
      (m : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    try_gen_recursively m ~kind:"transform_stm_list"
      ~if_stm:(If.try_gen_transform_stm_list ?filter)
      ~flow:(Flow.try_gen_transform_stm_list ?filter)

  let this_stm_if_ok (stm : Subject.Statement.t) ~(filter : Path_filter.t) :
      Path.Stm.t Opt_gen.t =
    Or_error.Let_syntax.(
      let%bind () =
        Or_error.tag
          (Path_filter.check_final_statement filter ~stm)
          ~tag:"Generated 'this-statement' path failed filtering checks"
      in
      Opt_gen.return Path.Stm.this_stm)

  let try_gen_transform_stm ?(filter : Path_filter.t = Path_filter.empty)
      (stm : Subject.Statement.t) : Path.Stm.t Opt_gen.t =
    let gen_base = this_stm_if_ok stm ~filter in
    let gen_rec =
      try_gen_recursively stm ~kind:"transform_stm"
        ~if_stm:(If.try_gen_transform_stm ~filter)
        ~flow:(Flow.try_gen_transform_stm ~filter)
    in
    Opt_gen.union [gen_base; gen_rec]
end

and Block :
  (S_producer with type t = Path.stm_list and type target = Subject.Block.t) =
struct
  type t = Path.stm_list

  let in_stm (index : int) : Path.Stm.t Opt_gen.t -> Path.Stms.t Opt_gen.t =
    Opt_gen.map ~f:(Path.Stms.in_stm index)

  type target = Subject.Block.t

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
      Fn.compose (List.concat_mapi ~f:try_gen_at) Act_fir.Block.statements
  end

  let try_gen_insert_stm ?(filter : Path_filter.t = Path_filter.empty)
      (dest : target) : Path.stm_list Opt_gen.t =
    let module I = Insert (struct
      let filter =
        Path_filter.update_with_block_metadata filter
          (Act_fir.Block.metadata dest)
    end) in
    Opt_gen.union (I.try_gen_here 0 :: I.try_gen_at_each dest)

  let gen_transform_stm_on ?(filter : Path_filter.t option) (index : int)
      (single_dest : Subject.Statement.t) : t Opt_gen.t =
    single_dest |> Statement.try_gen_transform_stm ?filter |> in_stm index

  let try_gen_transform_stm ?(filter : Path_filter.t = Path_filter.empty)
      (dest : target) : Path.stm_list Opt_gen.t =
    let filter =
      Path_filter.update_with_block_metadata filter
        (Act_fir.Block.metadata dest)
    in
    let stms = Act_fir.Block.statements dest in
    Opt_gen.union (List.mapi ~f:(gen_transform_stm_on ~filter) stms)

  let gen_transform_stm_list_here_populated (stms : Subject.Statement.t list)
      ~(filter : Path_filter.t) : t Q.Generator.t =
    let flt = filter in
    Q.Generator.(
      create (fun ~size ~random ->
          ignore size ;
          Option.value_exn (Act_utils.My_list.Random.stride stms ~random) )
      |> filter ~f:(fun (pos, len) ->
             Path_filter.are_final_statements_ok flt ~pos ~len ~all_stms:stms )
      >>| fun (p, d) -> Path.Stms.on_range p d)

  let gen_transform_stm_list_here (stms : Subject.Statement.t list)
      ~(filter : Path_filter.t) : t Q.Generator.t =
    let len = List.length stms in
    let after = Q.Generator.return (Path.Stms.on_range len 0) in
    let inner =
      if len = 0 then []
      else [gen_transform_stm_list_here_populated stms ~filter]
    in
    Q.Generator.union (after :: inner)

  let gen_transform_stm_list_on (index : int)
      (single_dest : Subject.Statement.t) ~(filter : Path_filter.t) :
      t Opt_gen.t =
    let f = filter in
    single_dest
    |> Statement.try_gen_transform_stm_list ~filter:f
    |> in_stm index

  let try_gen_transform_stm_list
      ?(filter : Path_filter.t = Path_filter.empty) (dest : target) :
      t Opt_gen.t =
    let filter =
      Path_filter.update_with_block_metadata filter
        (Act_fir.Block.metadata dest)
    in
    let stms = Act_fir.Block.statements dest in
    Opt_gen.union
      ( check_ok ~filter (gen_transform_stm_list_here ~filter stms)
      :: List.mapi stms ~f:(gen_transform_stm_list_on ~filter) )
end

and If :
  (S_producer
    with type t = Path.If.t
     and type target = Subject.Statement.If.t) = struct
  type t = Path.If.t

  type target = Subject.Statement.If.t

  let gen_opt_over_block ?(filter : Path_filter.t = Path_filter.empty)
      (branch : bool) (block : Subject.Block.t)
      ~(f :
         ?filter:Path_filter.t -> Subject.Block.t -> Path.Stms.t Opt_gen.t) :
      Path.If.t Opt_gen.t =
    Opt_gen.map (f ~filter block) ~f:(Path.If.in_branch branch)

  let gen_opt_over_blocks ?(filter : Path_filter.t option)
      (ifs : Metadata.t Stm.If.t)
      ~(f :
         ?filter:Path_filter.t -> Subject.Block.t -> Path.stm_list Opt_gen.t)
      : Path.If.t Opt_gen.t =
    Opt_gen.union
      [ gen_opt_over_block ?filter ~f true (Act_fir.If.t_branch ifs)
      ; gen_opt_over_block ?filter ~f false (Act_fir.If.f_branch ifs) ]

  let try_gen_insert_stm :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Block.try_gen_insert_stm

  let try_gen_transform_stm :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Block.try_gen_transform_stm

  let try_gen_transform_stm_list :
      ?filter:Path_filter.t -> Subject.Statement.If.t -> Path.If.t Opt_gen.t
      =
    gen_opt_over_blocks ~f:Block.try_gen_transform_stm_list
end

and Flow :
  (S_producer
    with type t = Path.Flow.t
     and type target = Subject.Statement.Flow.t) = struct
  type t = Path.Flow.t

  type target = Subject.Statement.Flow.t

  let gen_opt_over_body ?(filter : Path_filter.t = Path_filter.empty)
      (flow : Subject.Statement.Flow.t)
      ~(f :
         ?filter:Path_filter.t -> Subject.Block.t -> Path.Stms.t Opt_gen.t) :
      Path.Flow.t Opt_gen.t =
    let body = Act_fir.Flow_block.body flow in
    let flow = Act_fir.Statement_class.Flow.classify flow in
    let filter = Path_filter.update_with_flow ?flow filter in
    Opt_gen.map (f ~filter body) ~f:Path.Flow.in_body

  let try_gen_insert_stm :
         ?filter:Path_filter.t
      -> Subject.Statement.Flow.t
      -> Path.Flow.t Opt_gen.t =
    gen_opt_over_body ~f:Block.try_gen_insert_stm

  let try_gen_transform_stm :
         ?filter:Path_filter.t
      -> Subject.Statement.Flow.t
      -> Path.Flow.t Opt_gen.t =
    gen_opt_over_body ~f:Block.try_gen_transform_stm

  let try_gen_transform_stm_list :
         ?filter:Path_filter.t
      -> Subject.Statement.Flow.t
      -> Path.Flow.t Opt_gen.t =
    gen_opt_over_body ~f:Block.try_gen_transform_stm_list
end

module Thread :
  S_producer with type t = Path.Thread.t and type target = Subject.Thread.t =
struct
  type t = Path.Thread.t

  type target = Subject.Thread.t

  let gen_opt_over_stms ({stms; _} : target)
      ~(f : Subject.Block.t -> Path.stm_list Opt_gen.t) :
      Path.Thread.t Opt_gen.t =
    (* TODO(@MattWindsor91): get rid of this hack. *)
    let block = Subject.Block.make_existing ~statements:stms () in
    Opt_gen.map (f block) ~f:Path.Thread.in_stms

  let try_gen_insert_stm ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Block.try_gen_insert_stm ?filter)

  let try_gen_transform_stm ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Block.try_gen_transform_stm ?filter)

  let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
      target -> Path.Thread.t Opt_gen.t =
    gen_opt_over_stms ~f:(Block.try_gen_transform_stm_list ?filter)
end

type target = Subject.Test.t

let map_threads (test : target) ~(f : int -> Subject.Thread.t -> 'a option) :
    'a list =
  List.filter_mapi (Act_litmus.Test.Raw.threads test) ~f

let gen_opt_over_threads ?(filter : Path_filter.t = Path_filter.empty)
    (test : target)
    ~(f :
       ?filter:Path_filter.t -> Subject.Thread.t -> Path.Thread.t Opt_gen.t)
    : Path.Program.t Opt_gen.t =
  test
  |> map_threads ~f:(fun thread prog ->
         if Path_filter.is_thread_ok filter ~thread then
           Some
             (Opt_gen.map (f ~filter prog)
                ~f:(Path.Program.in_thread thread))
         else None )
  |> Opt_gen.union

let try_gen_insert_stm ?(filter : Path_filter.t option) :
    target -> Path.Program.t Opt_gen.t =
  gen_opt_over_threads ?filter ~f:Thread.try_gen_insert_stm

let try_gen_transform_stm ?(filter : Path_filter.t option) :
    target -> Path.Program.t Opt_gen.t =
  gen_opt_over_threads ?filter ~f:Thread.try_gen_transform_stm

let try_gen_transform_stm_list ?(filter : Path_filter.t option) :
    target -> Path.Program.t Opt_gen.t =
  gen_opt_over_threads ?filter ~f:Thread.try_gen_transform_stm_list
