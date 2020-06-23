(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module While = struct
  type t = Do_while | While [@@deriving sexp, compare, equal]
end

module Lock = struct
  type t = Atomic | Synchronized [@@deriving sexp, compare, equal]
end

(** {2 Headers proper} *)

module Header = struct
  type t = Lock of Lock.t | While of While.t * Expression.t
  [@@deriving sexp, compare, equal]

  let is_lock_block : t -> bool = function
    | Lock _ ->
        true
    | While _ ->
        false

  let is_while_loop : t -> bool = function
    | While _ ->
        true
    | Lock _ ->
        false

  (** Traversal over the expressions inside a header. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t := Expression.t = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Expression

    module On_monad (M : Monad.S) = struct
      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        match x with
        | Lock l ->
            M.return (Lock l)
        | While (w, e) ->
            M.(e |> f >>| fun e' -> While (w, e'))
    end
  end)
end

(** Opaque type of while loops. *)
type ('meta, 'stm) t = {header: Header.t; body: ('meta, 'stm) Block.t}
[@@deriving sexp, make, fields, compare, equal]

let while_loop ~(cond : Expression.t) ~(body : ('meta, 'stm) Block.t)
    ~(kind : While.t) : ('meta, 'stm) t =
  make ~body ~header:(Header.While (kind, cond))

let is_while_loop (x : ('meta, 'stm) t) : bool =
  Header.is_while_loop (header x)

let lock_block ~(body : ('meta, 'stm) Block.t) ~(kind : Lock.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.Lock kind)

let is_lock_block (x : ('meta, 'stm) t) : bool =
  Header.is_lock_block (header x)

module Base_map (A : Applicative.S) = struct
  let bmap (type m1 s1 m2 s2) (flow : (m1, s1) t)
      ~(header : Header.t -> Header.t A.t)
      ~(body : (m1, s1) Block.t -> (m2, s2) Block.t A.t) : (m2, s2) t A.t =
    let make header body = make ~header ~body in
    A.(return make <*> header flow.header <*> body flow.body)
end

module Bident = Base_map (Act_utils.Applicative.Ident)

let map = Bident.bmap
