(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Prim = struct
  type t =
    | Atomic of Atomic_class.t option
    | Early_out of Early_out.t option
    | Label
  [@@deriving compare, equal, sexp]

  let classify : Prim_statement.t -> t option =
    Prim_statement.value_map ~assign:(Fn.const None)
      ~atomic:(fun x -> Some (Atomic (Atomic_class.classify_stm x)))
      ~early_out:(fun x -> Some (Early_out (Some x)))
      ~goto:(Fn.const None) ~procedure_call:(Fn.const None)
      ~label:(Fn.const (Some Label)) ~nop:(Fn.const None)

  let classify_rec : Prim_statement.t -> t list =
    Class.lift_classify_rec classify

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Atomic None, Atomic _ | Early_out None, Early_out _ | Label, Label ->
        true
    | Atomic (Some template), Atomic (Some clazz) ->
        Atomic_class.matches clazz ~template
    | Early_out (Some e), Early_out (Some e') -> Early_out.equal e e'
    | _, _ -> false
end

module Flow = struct
  module Loop = struct
    type t = For | While of Flow_block.While.t option
    [@@deriving compare, equal, sexp]

    let class_matches (clazz : t) ~(template : t) : bool =
      match (template, clazz) with
      | For, For -> true
      | While (Some k1), While (Some k2) -> Flow_block.While.equal k1 k2
      | _, _ -> false
  end

  type t =
    | Loop of Loop.t option
    | Lock of Flow_block.Lock.t option
    | Explicit
    | Implicit
  [@@deriving compare, equal, sexp]

  let classify' ({header; _} : ('a, 'b) Flow_block.t) : t option =
    match header with
    | For _ -> Some (Loop (Some For))
    | Lock lk -> Some (Lock (Some lk))
    | While (wk, _) -> Some (Loop (Some (While (Some wk))))
    | Explicit -> Some Explicit
    | Implicit -> Some Implicit

  let classify (f : ('meta, 'meta Statement.t) Flow_block.t) : t option =
    classify' f

  let classify_rec (f : ('meta, 'meta Statement.t) Flow_block.t) : t list =
    Class.lift_classify_rec classify f

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Lock None, Lock _ | Loop None, Loop _ -> true
    | Lock (Some k1), Lock (Some k2) -> Flow_block.Lock.equal k1 k2
    | Loop (Some template), Loop (Some clazz) ->
        Loop.class_matches clazz ~template
    | _, _ -> false
end

type t = Prim of Prim.t option | If | Flow of Flow.t option
[@@deriving compare, equal, sexp]

include Class.Make_ext (struct
  type nonrec t = t

  type 'meta elt = 'meta Statement.t

  let classify (type e) (stm : e Statement.t) : t option =
    Statement.reduce_step stm
      ~prim:(fun {value; _} -> Some (Prim (Prim.classify value)))
      ~if_stm:(Fn.const (Some If))
      ~flow:(fun x -> Some (Flow (Flow.classify x)))

  let unfold_block (blk : ('m, t list) Block.t) : t list =
    Accessor.to_list (Block.each_statement @> Accessor.List.each) blk

  let classify_rec (type e) (stm : e Statement.t) : t list =
    Statement.reduce stm
      ~prim:(fun {value; _} ->
        List.map ~f:(fun t -> Prim (Some t)) (Prim.classify_rec value) )
      ~if_stm:(fun {t_branch; f_branch; _} ->
        If :: (unfold_block t_branch @ unfold_block f_branch) )
      ~flow:(fun f ->
        Option.to_list
          (Option.map ~f:(fun t -> Flow (Some t)) (Flow.classify' f))
        @ unfold_block f.body )

  let class_matches (clazz : t) ~(template : t) : bool =
    match (template, clazz) with
    | Prim None, Prim _ | If, If | Flow None, Flow _ -> true
    | Flow (Some template), Flow (Some clazz) ->
        Flow.class_matches clazz ~template
    | Prim (Some template), Prim (Some clazz) ->
        Prim.class_matches clazz ~template
    | _, _ -> false
end)

let atomic ?(specifically : Atomic_class.t option) () : t =
  Prim (Some (Atomic specifically))

let while_loop ?(specifically : Flow_block.While.t option) () : t =
  Flow (Some (Loop (Some (While specifically))))

let lock_block ?(specifically : Flow_block.Lock.t option) () : t =
  Flow (Some (Lock specifically))

let label : t = Prim (Some Label)
