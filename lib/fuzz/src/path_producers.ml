(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type 'm t = 'm Path_flag.Flagged.t Sequence.t

type ctx = Path_kind.t Path_context.t

(** Shorthand for the type of the recursive statement producing function. *)
type mu = Subject.Statement.t -> ctx:ctx -> Path.Stm.t t

module Helpers = struct
  (** [lift_err x] converts [x] into a sequence of one item if it is [Ok],
      and no items otherwise.i

      This is used to convert errors into blockages in the sequence. *)
  let lift_err : type m. m Or_error.t -> m Sequence.t = function
    | Ok o ->
        Sequence.singleton o
    | Error _ ->
        Sequence.empty

  (** [with_flags flags ~f ~ctx] registers [flags] in [ctx], then, if the
      result satisfies the current path filter, proceeds according to [f]. *)
  let with_flags (flags : Set.M(Path_flag).t) ~(f : ctx -> 'a t) ~(ctx : ctx)
      : 'a t =
    Sequence.(Path_context.add_flags ctx flags |> lift_err >>= f)

  (** [if_kind k x ~f ~ctx] is [f ~ctx x] if [ctx]'s kind is [k], and empty
      otherwise. *)
  let if_kind (k : Path_kind.t) (x : 'a) ~(f : 'a -> ctx:ctx -> 'b t)
      ~(ctx : ctx) : 'b t =
    if Path_kind.equal (Path_context.kind ctx) k then f x ~ctx
    else Sequence.empty

  (** [branch x branches] represents a branch in the path producer's decision
      tree, where an element [x] can produce paths using any of the
      generators in [branches]. *)
  let branch (x : 'a) (branches : ('a -> 'm t) list) : 'm t =
    branches |> Sequence.of_list |> Sequence.concat_map ~f:(fun f -> f x)

  let map_path (x : 'a t) ~(f : 'a -> 'b) : 'b t =
    Sequence.map x ~f:(Path_flag.Flagged.map_left ~f)
end

open Helpers

module Block = struct
  module Self_insert = struct
    (** [produce stms ~ctx] produces a statement-list path sequence producing
        every insert path targeting statements [stms]. *)
    let produce (stms : Subject.Statement.t list) ~(ctx : ctx) :
        Path.Stms.t t =
      (* All of these insertion sequences have the same metadata, and, so, we
         need only do the filter check once for all positions. *)
      Sequence.(
        lift_err (Path_context.check_filter_req ctx)
        >>= fun () ->
        (* Both ends are inclusive to let us insert at the end of the list. *)
        Sequence.range 0 ~start:`inclusive (List.length stms)
          ~stop:`inclusive
        |> Sequence.map ~f:(fun pos ->
               Path_context.lift_path ctx ~path:(Path.Stms.insert pos)))
  end

  module Self_transform_list = struct
    let make_on_range (i : int) (width : int) ~(ctx : ctx) :
        Path.Stms.t Path_flag.Flagged.t =
      Path_context.lift_path ctx ~path:(Path.Stms.on_range i width)

    (** [step i width ~stms ~ctx] produces one step of the transform-list
        path generator over [stms] and [ctx].

        This step considers expanding a [width]-long slice at [i] to a
        [width+1]-long slice. *)
    let step (i : int) (width : int) ~(stms : Subject.Statement.t list)
        ~(ctx : ctx) : (Path.Stms.t Path_flag.Flagged.t * int) option =
      let width' = width + 1 in
      Option.Let_syntax.(
        let%bind stm = List.nth stms (i + width) in
        let%map () = Result.ok (Path_context.check_filter_stm ctx ~stm) in
        (make_on_range i width' ~ctx, width'))

    (** [from i ~stms ~ctx] produces a statement-list path sequence producing
        every valid transform-list path targeting statement list [stms] and
        starting at position [i]. *)
    let from (i : int) ~(stms : Subject.Statement.t list) ~(ctx : ctx) :
        Path.Stms.t t =
      let ind = Sequence.unfold ~init:0 ~f:(step i ~stms ~ctx) in
      Sequence.shift_right ind (make_on_range i 0 ~ctx)

    (** [produce stms ~ctx] produces a statement-list path sequence producing
        every transform-list path targeting statements [stms]. *)
    let produce (stms : Subject.Statement.t list) ~(ctx : ctx) :
        Path.Stms.t t =
      Sequence.(
        lift_err (Path_context.check_filter_req ctx)
        >>= fun () ->
        (* As with insertion, both ends are inclusive; this is because
           transforming the 0 statements at the end of a list is always
           allowed. *)
        let len = List.length stms in
        let indices = List.range 0 ~start:`inclusive len ~stop:`inclusive in
        let seqs = List.map indices ~f:(from ~stms ~ctx) in
        Sequence.round_robin seqs)
  end

  (** [in_stm i s ~mu ~ctx] produces a statement-list path sequence recursing
      into statement number [i], with body [s]. *)
  let in_stm (i : int) (s : Subject.Statement.t) ~(mu : mu) ~(ctx : ctx) :
      Path.Stms.t t =
    s |> mu ~ctx |> map_path ~f:(Path.Stms.in_stm i)

  (** [in_stms b ~mu ~ctx] produces a statement-list path sequence recursing
      into every statement in [b]. *)
  let in_stms (xs : Subject.Statement.t list) ~(mu : mu) ~(ctx : ctx) :
      Path.Stms.t t =
    xs |> List.mapi ~f:(in_stm ~mu ~ctx) |> Sequence.round_robin

  let produce_stms (b : Subject.Statement.t list) ~(mu : mu) ~(ctx : ctx) :
      Path.Stms.t t =
    branch b
      [ if_kind Insert ~f:Self_insert.produce ~ctx
      ; if_kind Transform_list ~f:Self_transform_list.produce ~ctx
        (* All 'transform' targets are inside this block's statements. *)
      ; in_stms ~ctx ~mu ]

  let produce (b : Subject.Block.t) ~(mu : mu) : ctx:ctx -> Path.Stms.t t =
    with_flags (Path_flag.flags_of_block b) ~f:(fun ctx ->
        produce_stms (Act_fir.Block.statements b) ~mu ~ctx)
end

(** If blocks and flow blocks both share the same path structure, with some
    minor differences relating to branching. This functor abstracts over
    both. *)
module Make_flow (F : sig
  (** Type of targets. *)
  type t

  (** Type of branches. *)
  type branch [@@deriving enumerate]

  val sel_branch : branch -> t -> Subject.Block.t
  (** [sel_branch branch x] focuses down on the branch [branch] in [x]. *)

  val lift_path : branch -> Path.Stms.t -> Path.Stm.t
  (** [lift_path branch p] lifts the block path [b] into a statement path,
      going through branch [branch]. *)

  val thru_flags : t -> Set.M(Path_flag).t
  (** [thru_flags x] gets any flags that should activate on passing through
      [x]. *)
end) =
struct
  let produce_branch (b : F.branch) (i : F.t) ~(mu : mu) :
      ctx:ctx -> Path.Stm.t t =
    with_flags (F.thru_flags i) ~f:(fun ctx ->
        i |> F.sel_branch b |> Block.produce ~mu ~ctx
        |> map_path ~f:(F.lift_path b))

  let produce (i : F.t) ~(mu : mu) ~(ctx : ctx) : Path.Stm.t t =
    (* No metadata on if statement headers. *)
    branch i (List.map F.all_of_branch ~f:(produce_branch ~mu ~ctx))
end

module If = Make_flow (struct
  type t = Subject.Statement.If.t

  type branch = bool [@@deriving enumerate]

  let sel_branch (b : bool) : Subject.Statement.If.t -> Subject.Block.t =
    if b then Act_fir.If.t_branch else Act_fir.If.f_branch

  let lift_path b rest = Path.Stm.in_if @@ Path.If.in_branch b @@ rest

  let thru_flags _ = Set.empty (module Path_flag)
end)

module Flow = Make_flow (struct
  type t = Subject.Statement.Flow.t

  type branch = unit [@@deriving enumerate]

  let sel_branch () = Act_fir.Flow_block.body

  let lift_path () rest = Path.Stm.in_flow @@ Path.Flow.in_body @@ rest

  let thru_flags = Path_flag.flags_of_flow
end)

module Stm = struct
  let self (stm : Subject.Statement.t) ~(ctx : ctx) : Path.Stm.t t =
    Sequence.(
      lift_err (Path_context.check_filter_req ctx)
      >>= fun () ->
      lift_err (Path_context.check_filter_stm ctx ~stm)
      >>| fun () -> Path_context.lift_path ctx ~path:Path.Stm.this_stm)

  let recursive (s : Subject.Statement.t) ~(mu : mu) ~(ctx : ctx) :
      Path.Stm.t t =
    Act_fir.Statement.reduce_step s ~prim:(Fn.const Sequence.empty)
      ~if_stm:(If.produce ~mu ~ctx) ~flow:(Flow.produce ~mu ~ctx)

  let produce (s : Subject.Statement.t) ~(ctx : ctx) : Path.Stm.t t =
    let rec mu s =
      with_flags (Path_flag.flags_of_stm s) ~f:(fun ctx ->
          branch s [if_kind Transform ~f:self ~ctx; recursive ~mu ~ctx])
    in
    mu s ~ctx
end

let thread (tid : int) (s : Subject.Thread.t) ~(ctx : ctx) : Path.t t =
  Sequence.(
    lift_err (Path_context.check_thread_ok ctx ~thread:tid)
    >>= fun () ->
    s.stms
    |> Block.produce_stms ~ctx ~mu:Stm.produce
    |> map_path ~f:(Fn.compose (Path.in_thread tid) Path.Thread.in_stms))

let produce (test : Subject.Test.t) ~(ctx : ctx) : Path.t t =
  test |> Act_litmus.Test.Raw.threads
  |> List.mapi ~f:(thread ~ctx)
  |> Sequence.round_robin

let produce_seq ?(filter : Path_filter.t option) (test : Subject.Test.t)
    ~(kind : Path_kind.t) : Path.t Path_flag.Flagged.t Sequence.t =
  let ctx = Path_context.init kind ?filter in
  produce test ~ctx

let is_constructible ?(filter : Path_filter.t option) (test : Subject.Test.t)
    ~(kind : Path_kind.t) : bool =
  not (Sequence.is_empty (produce_seq test ?filter ~kind))

let try_gen_with_flags ?(filter : Path_filter.t option)
    (test : Subject.Test.t) ~(kind : Path_kind.t) :
    Path.t Path_flag.Flagged.t Opt_gen.t =
  match Sequence.to_list_rev (produce_seq test ~kind ?filter) with
  | [] ->
      Or_error.error_string "No valid paths generated"
  | xs ->
      Ok (Base_quickcheck.Generator.of_list xs)

let try_gen ?(filter : Path_filter.t option) (test : Subject.Test.t)
    ~(kind : Path_kind.t) : Path.t Opt_gen.t =
  Opt_gen.map
    (try_gen_with_flags ?filter ~kind test)
    ~f:Path_flag.Flagged.path
