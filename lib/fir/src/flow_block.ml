(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module For = struct
  module Direction = struct
    type t = Down_exclusive | Down_inclusive | Up_exclusive | Up_inclusive
    [@@deriving sexp, compare, equal]
  end

  type t =
    { lvalue: Lvalue.t
    ; init_value: Expression.t
    ; cmp_value: Expression.t
    ; direction: Direction.t }
  [@@deriving accessors, make, sexp, compare, equal]

  let exprs_many (x : t) : (t, Expression.t, Expression.t) Accessor.Many.t =
    Accessor.Many.(
      map2 (access x.init_value) (access x.cmp_value)
        ~f:(fun init_value' cmp_value' ->
          {x with init_value= init_value'; cmp_value= cmp_value'}))

  let exprs : type i. (i, Expression.t, t, [< many]) Accessor.Simple.t =
    [%accessor Accessor.many exprs_many]
end

module While = struct
  module M = struct
    type t = Do_while | While [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [(Do_while, "do-while"); (While, "while")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

module Lock = struct
  type t = Atomic | Synchronized [@@deriving sexp, compare, equal]
end

(** {2 Headers proper} *)

module Header = struct
  type t =
    | For of For.t
    | Lock of Lock.t
    | While of While.t * Expression.t
    | Explicit
    | Implicit
  [@@deriving accessors, sexp, compare, equal]

  let exprs_many : t -> (t, Expression.t, Expression.t) Accessor.Many.t =
    Accessor.Many.(
      function
      | For f ->
          For.exprs_many f >>| fun f' -> For f'
      | While (w, e) ->
          access e >>| fun e' -> While (w, e')
      | (Explicit | Implicit | Lock _) as x ->
          return x)

  let exprs : type i. (i, Expression.t, t, [< many]) Accessor.Simple.t =
    [%accessor Accessor.many exprs_many]

  (** Traversal over the expressions inside a header. *)
  module On_expressions :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t := Expression.t = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Expression

    module On_monad (M : Monad.S) = struct
      module AccM = Accessor.Of_monad (struct
        include M

        let apply = `Define_using_bind
      end)

      let map_m : t -> f:(Elt.t -> Elt.t M.t) -> t M.t = AccM.map exprs
    end
  end)
end

(** Opaque type of while loops. *)
type ('meta, 'stm) t = {header: Header.t; body: ('meta, 'stm) Block.t}
[@@deriving sexp, make, fields, compare, equal]

let for_loop ~(control : For.t) ~(body : ('meta, 'stm) Block.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.For control)

let while_loop ~(cond : Expression.t) ~(body : ('meta, 'stm) Block.t)
    ~(kind : While.t) : ('meta, 'stm) t =
  make ~body ~header:(Header.While (kind, cond))

let lock_block ~(body : ('meta, 'stm) Block.t) ~(kind : Lock.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.Lock kind)

let explicit (body : ('meta, 'stm) Block.t) : ('meta, 'stm) t =
  make ~body ~header:Explicit

let implicit (body : ('meta, 'stm) Block.t) : ('meta, 'stm) t =
  make ~body ~header:Implicit

module Base_map (A : Applicative.S) = struct
  let bmap (type m1 s1 m2 s2) (flow : (m1, s1) t)
      ~(header : Header.t -> Header.t A.t)
      ~(body : (m1, s1) Block.t -> (m2, s2) Block.t A.t) : (m2, s2) t A.t =
    let make header body = make ~header ~body in
    A.(return make <*> header flow.header <*> body flow.body)
end

module Bident = Base_map (Act_utils.Applicative.Ident)

let map = Bident.bmap
