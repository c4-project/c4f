(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let const_some (x : 'a) (_ : 'b) : 'a option = Some x

module Atomic = struct
  module M = struct
    type t = Cmpxchg | Fence | Fetch | Load | Store | Xchg
    [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [ (Cmpxchg, "cmpxchg")
      ; (Fence, "fence")
      ; (Fetch, "fetch")
      ; (Load, "load")
      ; (Store, "store")
      ; (Xchg, "xchg") ]
  end

  include M
  include Act_utils.Enum.Extend_table (M)

  let classify : Atomic_statement.t -> t option =
    Atomic_statement.reduce ~cmpxchg:(const_some Cmpxchg)
      ~fence:(const_some Fence) ~fetch:(const_some Fetch)
      ~store:(const_some Store) ~xchg:(const_some Xchg)

  let matches (clazz : t) ~(template : t) : bool = equal clazz template

  let atomic_matches (atom : Atomic_statement.t) ~(template : t) : bool =
    Option.exists (classify atom) ~f:(matches ~template)
end

module Prim = struct
  type t = Atomic of Atomic.t option [@@deriving compare, equal, sexp]

  let classify : Prim_statement.t -> t option =
    Prim_statement.reduce ~assign:(Fn.const None)
      ~atomic:(fun x -> Some (Atomic (Atomic.classify x)))
      ~early_out:(Fn.const None) ~goto:(Fn.const None)
      ~procedure_call:(Fn.const None) ~label:(Fn.const None)
      ~nop:(Fn.const None)

  let matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Atomic None, Atomic _ ->
        true
    | Atomic (Some template), Atomic (Some clazz) ->
        Atomic.matches clazz ~template
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

let atomic ?(specifically : Atomic.t option) () : t =
  Prim (Some (Prim.Atomic specifically))
