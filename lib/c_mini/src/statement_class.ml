(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Atomic = struct
  type t =
  | Store
    [@@deriving equal, sexp]

  let classify : Atomic_statement.t -> t option =
    Atomic_statement.reduce
    ~cmpxchg:(Fn.const None)
    ~fence:(Fn.const None)
    ~fetch:(Fn.const None)
    ~store:(Fn.const (Some Store))
    ~xchg:(Fn.const None)

    let matches (clazz : t) ~(template : t) : bool =
      ignore clazz; ignore template; true
end

module Prim = struct
  type t =
  | Atomic of Atomic.t option
    [@@deriving equal, sexp]

  let classify : Prim_statement.t -> t option =
    Prim_statement.reduce
    ~assign:(Fn.const None)
    ~atomic:(fun x -> Some (Atomic (Atomic.classify x)))
    ~early_out:(Fn.const None)
    ~goto:(Fn.const None)
    ~procedure_call:(Fn.const None)
    ~label:(Fn.const None)
    ~nop:(Fn.const None)

  let matches (clazz : t) ~(template : t) : bool =
    match template, clazz with
    | Atomic None, Atomic _ -> true
    | Atomic (Some template), Atomic (Some clazz) -> Atomic.matches clazz ~template
    | _, _ -> false
end

type t =
  | Prim of Prim.t option
  | If
  [@@deriving equal, sexp]

let classify (type e) (stm : e Statement.t) : t option =
  Statement.reduce stm
    ~prim:(fun (_, x) -> Some (Prim (Prim.classify x)))
    ~if_stm:(Fn.const (Some If))
    ~while_loop:(Fn.const None)

let matches (clazz : t) ~(template : t) : bool =
  match template, clazz with
  | Prim None, Prim _ | If, If -> true
  | Prim (Some template), Prim (Some clazz) -> Prim.matches clazz ~template
  | _, _ -> false
