(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module For = struct
  module Direction = struct
    type t = Down_exclusive | Down_inclusive | Up_exclusive | Up_inclusive
    [@@deriving sexp, compare, equal]
  end

  module Access = struct
    type t =
      { lvalue: Lvalue.t
      ; init_value: Expression.t
      ; cmp_value: Expression.t
      ; direction: Direction.t }
    [@@deriving accessors, make, sexp, compare, equal]
  end

  include Access

  let on_expressions_many (x : t) :
      (t, Expression.t, Expression.t) Accessor.Many.t =
    Accessor.Many.(
      map2 (access x.init_value) (access x.cmp_value)
        ~f:(fun init_value' cmp_value' ->
          {x with init_value= init_value'; cmp_value= cmp_value'}))

  let on_expressions :
      (unit -> Expression.t -> Expression.t, _ -> t -> t, _) Accessor.t =
    [%accessor Accessor.many on_expressions_many]

  let lvalue = Accessor.get lvalue

  let init_value = Accessor.get init_value

  let cmp_value = Accessor.get cmp_value

  let direction = Accessor.get direction
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
  type t = For of For.t | Lock of Lock.t | While of While.t * Expression.t
  [@@deriving accessors, sexp, compare, equal]

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

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        match x with
        | For fr ->
            M.(AccM.map For.on_expressions fr ~f >>| fun x -> For x)
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

let for_loop ~(control : For.t) ~(body : ('meta, 'stm) Block.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.For control)

let while_loop ~(cond : Expression.t) ~(body : ('meta, 'stm) Block.t)
    ~(kind : While.t) : ('meta, 'stm) t =
  make ~body ~header:(Header.While (kind, cond))

let lock_block ~(body : ('meta, 'stm) Block.t) ~(kind : Lock.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.Lock kind)

module Base_map (A : Applicative.S) = struct
  let bmap (type m1 s1 m2 s2) (flow : (m1, s1) t)
      ~(header : Header.t -> Header.t A.t)
      ~(body : (m1, s1) Block.t -> (m2, s2) Block.t A.t) : (m2, s2) t A.t =
    let make header body = make ~header ~body in
    A.(return make <*> header flow.header <*> body flow.body)
end

module Bident = Base_map (Act_utils.Applicative.Ident)

let map = Bident.bmap
