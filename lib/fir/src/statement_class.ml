open Base

module Prim = struct
  type t = Atomic of Atomic_class.t option | Label
  [@@deriving compare, equal, sexp]

  let classify : Prim_statement.t -> t option =
    Prim_statement.value_map ~assign:(Fn.const None)
      ~atomic:(fun x -> Some (Atomic (Atomic_class.classify_stm x)))
      ~early_out:(Fn.const None) ~goto:(Fn.const None)
      ~procedure_call:(Fn.const None) ~label:(Fn.const (Some Label))
      ~nop:(Fn.const None)

  let classify_rec : Prim_statement.t -> t list =
    Class.lift_classify_rec classify

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Atomic None, Atomic _ | Label, Label ->
        true
    | Atomic (Some template), Atomic (Some clazz) ->
        Atomic_class.matches clazz ~template
    | _, _ ->
        false
end

module Flow = struct
  type t =
    | Lock of Flow_block.Lock.t option
    | While of Flow_block.While.t option
  [@@deriving compare, equal, sexp]

  let classify' (f : ('a, 'b) Flow_block.t) : t option =
    match Flow_block.header f with
    | Lock lk ->
        Some (Lock (Some lk))
    | While (wk, _) ->
        Some (While (Some wk))

  let classify (f : ('meta, 'meta Statement.t) Flow_block.t) : t option =
    classify' f

  let classify_rec (f : ('meta, 'meta Statement.t) Flow_block.t) : t list =
    Class.lift_classify_rec classify f

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Lock None, Lock _ | While None, While _ ->
        true
    | Lock (Some k1), Lock (Some k2) ->
        Flow_block.Lock.equal k1 k2
    | While (Some k1), While (Some k2) ->
        Flow_block.While.equal k1 k2
    | _, _ ->
        false
end

module Main = struct
  type t = Prim of Prim.t option | If | Flow of Flow.t option
  [@@deriving compare, equal, sexp]

  type 'meta elt = 'meta Statement.t

  let classify (type e) (stm : e Statement.t) : t option =
    Statement.reduce_step stm
      ~prim:(fun (_, x) -> Some (Prim (Prim.classify x)))
      ~if_stm:(Fn.const (Some If))
      ~flow:(fun x -> Some (Flow (Flow.classify x)))

  let unfold_block (blk : ('m, t list) Block.t) : t list =
    List.concat (Block.statements blk)

  let classify_rec (type e) (stm : e Statement.t) : t list =
    Statement.reduce stm
      ~prim:(fun (_, x) ->
        List.map ~f:(fun t -> Prim (Some t)) (Prim.classify_rec x) )
      ~if_stm:(fun ifs ->
        If
        :: (unfold_block (If.t_branch ifs) @ unfold_block (If.f_branch ifs))
        )
      ~flow:(fun f ->
        Option.to_list
          (Option.map ~f:(fun t -> Flow (Some t)) (Flow.classify' f))
        @ unfold_block (Flow_block.body f) )

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Prim None, Prim _ | If, If | Flow None, Flow _ ->
        true
    | Flow (Some template), Flow (Some clazz) ->
        Flow.class_matches clazz ~template
    | Prim (Some template), Prim (Some clazz) ->
        Prim.class_matches clazz ~template
    | _, _ ->
        false
end

include Main
include Class.Make_ext (Main)

let atomic ?(specifically : Atomic_class.t option) () : t =
  Prim (Some (Atomic specifically))

let while_loop ?(specifically : Flow_block.While.t option) () : t =
  Flow (Some (While specifically))

let lock_block ?(specifically : Flow_block.Lock.t option) () : t =
  Flow (Some (Lock specifically))

let label : t = Prim (Some Label)
