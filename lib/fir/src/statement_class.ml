open Base

module Prim = struct
  type t = Atomic of Atomic_class.t option
  [@@deriving compare, equal, sexp]

  let classify : Prim_statement.t -> t option =
    Prim_statement.reduce ~assign:(Fn.const None)
      ~atomic:(fun x -> Some (Atomic (Atomic_class.classify_stm x)))
      ~early_out:(Fn.const None) ~goto:(Fn.const None)
      ~procedure_call:(Fn.const None) ~label:(Fn.const None)
      ~nop:(Fn.const None)

  let matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Atomic None, Atomic _ ->
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

  let classify (f : (_, _) Flow_block.t) : t option =
    match Flow_block.header f with
    | Lock lk ->
        Some (Lock (Some lk))
    | While (wk, _) ->
        Some (While (Some wk))

  let matches (clazz : t) ~(template : t) : bool =
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

type t = Prim of Prim.t option | If | Flow of Flow.t option
[@@deriving compare, equal, sexp]

let classify (type e) (stm : e Statement.t) : t option =
  Statement.reduce stm
    ~prim:(fun (_, x) -> Some (Prim (Prim.classify x)))
    ~if_stm:(Fn.const (Some If))
    ~flow:(fun x -> Some (Flow (Flow.classify x)))

let matches (clazz : t) ~(template : t) : bool =
  match (template, clazz) with
  | Prim None, Prim _ | If, If | Flow None, Flow _ ->
      true
  | Flow (Some template), Flow (Some clazz) ->
      Flow.matches clazz ~template
  | Prim (Some template), Prim (Some clazz) ->
      Prim.matches clazz ~template
  | _, _ ->
      false

let sum_block (type e) (blk : (e, int) Block.t) : int =
  List.sum (module Int) ~f:Fn.id (Block.statements blk)

let one_if_matches (clazz : t) ~(template : t) : int =
  clazz |> matches ~template |> Bool.to_int

let count_matches (type e) (stm : e Statement.t) ~(template : t) : int =
  Statement.reduce stm
    ~prim:(fun (_, x) -> one_if_matches (Prim (Prim.classify x)) ~template)
    ~if_stm:(fun ifs ->
      one_if_matches If ~template
      + sum_block (If.t_branch ifs)
      + sum_block (If.f_branch ifs))
    ~flow:(fun f ->
      one_if_matches (Flow (Flow.classify f)) ~template
      + sum_block (Flow_block.body f))

let atomic ?(specifically : Atomic_class.t option) () : t =
  Prim (Some (Prim.Atomic specifically))

let while_loop ?(specifically : Flow_block.While.t option) () : t =
  Flow (Some (Flow.While specifically))
