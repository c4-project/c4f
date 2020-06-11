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

type t = Prim of Prim.t option | If | While of While.Kind.t option
[@@deriving compare, equal, sexp]

let classify_while (w : (_, _) While.t) : t = While (Some (While.kind w))

let classify (type e) (stm : e Statement.t) : t option =
  Statement.reduce stm
    ~prim:(fun (_, x) -> Some (Prim (Prim.classify x)))
    ~if_stm:(Fn.const (Some If))
    ~while_loop:(fun x -> Some (classify_while x))

let matches (clazz : t) ~(template : t) : bool =
  match (template, clazz) with
  | Prim None, Prim _ | If, If | While None, While _ ->
      true
  | While (Some k1), While (Some k2) ->
      While.Kind.equal k1 k2
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
    ~while_loop:(fun w ->
      one_if_matches (classify_while w) ~template + sum_block (While.body w))

let atomic ?(specifically : Atomic_class.t option) () : t =
  Prim (Some (Prim.Atomic specifically))
