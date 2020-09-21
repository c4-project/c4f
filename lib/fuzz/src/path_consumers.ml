(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type ctx = Path_kind.With_action.t Path_context.t

(** Shorthand for the type of the recursive statement producing function. *)
type mu =
     Subject.Statement.t
  -> path:Path.Stm.t
  -> ctx:ctx
  -> Subject.Statement.t Or_error.t

module Helpers = struct
  let with_flags (flags : Set.M(Path_flag).t) ~(f : ctx -> 'a Or_error.t)
      ~(ctx : ctx) : 'a Or_error.t =
    Or_error.(Path_context.add_flags ctx flags >>= f)

  let bad_kind (got_wa : Path_kind.With_action.t) ~(want : Path_kind.t) :
      'a Or_error.t =
    let got = Path_kind.With_action.to_kind got_wa in
    Or_error.error_s
      [%message
        "Unexpected kind of action associated with this path"
          ~got:(got : Path_kind.t)
          ~want:(want : Path_kind.t)]

  let bad_stm (got_stm : Subject.Statement.t) ~(want : Fir.Statement_class.t)
      : 'a Or_error.t =
    let got = Fir.Statement_class.classify got_stm in
    Or_error.error_s
      [%message
        "Unexpected statement class for this path"
          ~got:(got : Fir.Statement_class.t option)
          ~want:(want : Fir.Statement_class.t)]
end

let checked_transform (stm : Subject.Statement.t) ~(ctx : ctx)
    ~(f : Subject.Statement.t -> Subject.Statement.t Or_error.t)
    ~(tag : string) : Subject.Statement.t Or_error.t =
  Or_error.tag ~tag
    Or_error.(
      Let_syntax.(
        let%bind () =
          tag (Path_context.check_filter_req ctx) ~tag:"while checking flags"
        in
        let%bind () =
          tag
            (Path_context.check_filter_stm ctx ~stm)
            ~tag:"while checking statements"
        in
        f stm))

open Helpers

module Block = struct
  let insert (b : Subject.Statement.t list) ~(pos : int) ~(ctx : ctx) :
      Subject.Statement.t list Or_error.t =
    (* TODO(@MattWindsor91): consider unifying with Transform_list; the
       similarity of the code here makes it very clear that the differences
       between the two could be a path filter. *)
    match Path_context.kind ctx with
    | Insert stms ->
        Or_error.(
          tag
            (Path_context.check_filter_req ctx)
            ~tag:"checking flags on insertion"
          >>= fun () ->
          Act_utils.My_list.splice b ~pos ~len:0 ~replace_f:(Fn.const stms))
    | x ->
        bad_kind x ~want:Insert

  let checked_transform_list_on_range (stms : Subject.Statement.t list)
      ~(ctx : ctx)
      ~(f : Subject.Statement.t list -> Subject.Statement.t list Or_error.t)
      : Subject.Statement.t list Or_error.t =
    Or_error.(
      tag ~tag:"in on-range transform-list"
        Let_syntax.(
          let%bind () =
            tag
              (Path_context.check_filter_req ctx)
              ~tag:"while checking flags"
          in
          let%bind () =
            tag
              (Path_context.check_filter_stms ctx ~stms)
              ~tag:"while checking statements"
          in
          f stms))

  let on_range (b : Subject.Statement.t list) ~(pos : int) ~(len : int)
      ~(ctx : ctx) : Subject.Statement.t list Or_error.t =
    match Path_context.kind ctx with
    | Transform f ->
        Act_utils.My_list.try_map_sub b ~pos ~len
          ~f:(checked_transform ~ctx ~f ~tag:"in on-range transform")
    | Transform_list f ->
        Act_utils.My_list.try_splice b ~pos ~len
          ~replace_f:(checked_transform_list_on_range ~ctx ~f)
    | x ->
        bad_kind x ~want:Transform_list

  let in_stm (b : Subject.Statement.t list) ~(pos : int) ~(path : Path.Stm.t)
      ~(mu : mu) ~(ctx : ctx) : Subject.Statement.t list Or_error.t =
    Tx.List.With_errors.replace_m b pos
      ~f:Or_error.(fun s -> s |> mu ~path ~ctx >>| Option.return)

  let consume_stms (b : Subject.Statement.t list) ~(path : Path.Stms.t)
      ~(mu : mu) ~(ctx : ctx) : Subject.Statement.t list Or_error.t =
    Or_error.Let_syntax.(
      let%bind () =
        Path_context.check_anchor ctx ~block_len:(List.length b) ~path
      in
      match path with
      | Insert pos ->
          insert b ~pos ~ctx
      | On_range (pos, len) ->
          on_range b ~pos ~len ~ctx
      | In_stm (pos, path) ->
          in_stm b ~pos ~path ~mu ~ctx)

  let consume (b : Subject.Block.t) ~(path : Path.Stms.t) ~(mu : mu) :
      ctx:ctx -> Subject.Block.t Or_error.t =
    let metadata = b.@(Fir.Block.metadata) in
    with_flags (Path_flag.flags_of_block b) ~f:(fun ctx ->
        Or_error.Let_syntax.(
          let%map statements =
            consume_stms b.@(Fir.Block.statements) ~path ~mu ~ctx
          in
          Fir.Block.make ~statements ~metadata ()))
end

(** If blocks and flow blocks both share the same path structure, with some
    minor differences relating to branching. This functor abstracts over
    both. *)
module Make_flow (F : sig
  (** Type of targets. *)
  type t

  (** Type of the 'rest' of the path leading to this flow, containing both
      the remainder path and any block selectors. *)
  type rest

  val path : rest -> Path.Stms.t
  (** [path rest] extracts the path from [rest]. *)

  val thru_flags : t -> Set.M(Path_flag).t
  (** [thru_flags x] gets any flags that should activate on passing through
      [x]. *)

  val block_lens :
       rest
    -> (Subject.Block.t -> Subject.Block.t Or_error.t)
    -> t
    -> t Or_error.t
  (** [block_lens rest f flow] lifts [f] onto the block pointed to by [rest],
      and applies it to [flow]. *)
end) =
struct
  let this_cond (_ : F.t) ~(ctx : ctx) : F.t Or_error.t =
    ignore ctx ;
    Or_error.error_string "no this-cond paths implemented yet"

  let in_block (x : F.t) ~(rest : F.rest) ~(mu : mu) ~(ctx : ctx) :
      F.t Or_error.t =
    let path = F.path rest in
    F.block_lens rest (Block.consume ~path ~mu ~ctx) x

  let consume (x : F.t) ~(path : F.rest Path.flow_block) ~(mu : mu) :
      ctx:ctx -> F.t Or_error.t =
    with_flags (F.thru_flags x) ~f:(fun ctx ->
        match path with
        | This_cond ->
            this_cond x ~ctx
        | In_block rest ->
            in_block x ~rest ~mu ~ctx)
end

module If = Make_flow (struct
  type t = Subject.Statement.If.t

  type rest = bool * Path.Stms.t

  let path : rest -> Path.Stms.t = snd

  let thru_flags = Fn.const (Set.empty (module Path_flag))

  module Map = Fir.If.Base_map (Or_error)

  let block_lens ((b, _) : rest)
      (f : Subject.Block.t -> Subject.Block.t Or_error.t) : t -> t Or_error.t
      =
    let br active block = if active then f block else Ok block in
    Map.bmap ~cond:Or_error.return ~t_branch:(br b) ~f_branch:(br (not b))
end)

module Flow = Make_flow (struct
  type t = Subject.Statement.Flow.t

  type rest = Path.Stms.t

  let path : rest -> Path.Stms.t = Fn.id

  let thru_flags = Path_flag.flags_of_flow

  module Map = Fir.Flow_block.Base_map (Or_error)

  let block_lens (_ : rest)
      (f : Subject.Block.t -> Subject.Block.t Or_error.t) : t -> t Or_error.t
      =
    Map.bmap ~header:Or_error.return ~body:f
end)

module Stm = struct
  module Map = Fir.Statement_traverse.Base_map (Or_error)

  let this_stm (stm : Subject.Statement.t) ~(ctx : ctx) :
      Subject.Statement.t Or_error.t =
    match Path_context.kind ctx with
    | Transform f ->
        checked_transform stm ~ctx ~f ~tag:"on transform"
    | k ->
        bad_kind k ~want:Transform

  let in_flow (s : Subject.Statement.t) ~(path : Path.Flow.t) ~(mu : mu)
      ~(ctx : ctx) : Subject.Statement.t Or_error.t =
    let bad _ = bad_stm s ~want:(Flow None) in
    Map.bmap s ~prim:bad ~flow:(Flow.consume ~path ~mu ~ctx) ~if_stm:bad

  let in_if (s : Subject.Statement.t) ~(path : Path.If.t) ~(mu : mu)
      ~(ctx : ctx) : Subject.Statement.t Or_error.t =
    let bad _ = bad_stm s ~want:If in
    Map.bmap s ~prim:bad ~flow:bad ~if_stm:(If.consume ~path ~mu ~ctx)

  let consume (s : Subject.Statement.t) ~(path : Path.Stm.t) ~(ctx : ctx) :
      Subject.Statement.t Or_error.t =
    let rec mu s ~(path : Path.Stm.t) =
      with_flags (Path_flag.flags_of_stm s) ~f:(fun ctx ->
          match path with
          | This_stm ->
              this_stm s ~ctx
          | In_flow path ->
              in_flow s ~path ~mu ~ctx
          | In_if path ->
              in_if s ~path ~mu ~ctx)
    in
    mu s ~path ~ctx
end

let thread (tid : int) (s : Subject.Thread.t) ~(path : Path.Thread.t)
    ~(ctx : ctx) : Subject.Thread.t Or_error.t =
  Or_error.(
    Path_context.check_thread_ok ctx ~thread:tid
    >>= fun () ->
    match path with
    | In_stms path ->
        s.stms
        |> Block.consume_stms ~path ~ctx ~mu:Stm.consume
        >>| fun stms' -> {s with stms= stms'})

let consume (test : Subject.Test.t) ~(path : Path.t) ~(ctx : ctx) :
    Subject.Test.t Or_error.t =
  match path with
  | In_thread (index, path) ->
      Act_litmus.Test.Raw.try_map_thread test ~index
        ~f:(thread index ~path ~ctx)

let consume_with_flags ?(filter : Path_filter.t option)
    (test : Subject.Test.t) ~(path : Path.t Path_flag.Flagged.t)
    ~(action : Path_kind.With_action.t) : Subject.Test.t Or_error.t =
  let {Path_flag.Flagged.path; flags; _} = path in
  let filter =
    Path_filter.(Option.merge ~f:( + ) filter (Some (require_flags flags)))
  in
  let ctx = Path_context.init action ?filter in
  consume test ~path ~ctx
