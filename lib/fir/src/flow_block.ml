(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module For = struct
  type t =
    {init: Assign.t option; cmp: Expression.t option; update: Assign.t option}
  [@@deriving accessors, make, sexp, compare, equal]

  module AMany = Accessor.Of_applicative2 (struct
    include Accessor.Many

    type nonrec ('a, 'b) t = ('a, Expression.t, 'b) t
  end)

  let init_opt : ('i, Assign.t, t, [< optional]) Accessor.Simple.t =
    [%accessor init @> Accessor.Option.some]

  let cmp_opt : ('i, Expression.t, t, [< optional]) Accessor.Simple.t =
    [%accessor cmp @> Accessor.Option.some]

  let update_opt : ('i, Assign.t, t, [< optional]) Accessor.Simple.t =
    [%accessor update @> Accessor.Option.some]

  let exprs_many (x : t) : (t, Expression.t, Expression.t) Accessor.Many.t =
    Accessor.Many.(
      return (fun init cmp update -> make ?init ?cmp ?update ())
      <*> AMany.map (Accessor.Option.some @> Assign.exprs) ~f:access x.init
      <*> AMany.map Accessor.Option.some ~f:access x.cmp
      <*> AMany.map (Accessor.Option.some @> Assign.exprs) ~f:access x.update)

  let exprs : type i. (i, Expression.t, t, [< many]) Accessor.Simple.t =
    [%accessor Accessor.many exprs_many]

  module Simple = struct
    module Inclusivity = struct
      type t = Exclusive | Inclusive
      [@@deriving sexp, compare, equal, quickcheck]
    end

    module Direction = struct
      type t = Down of Inclusivity.t | Up of Inclusivity.t
      [@@deriving sexp, compare, equal, quickcheck]

      let src : t -> Assign.Source.t = function
        | Up Exclusive | Up Inclusive ->
            Inc
        | Down Exclusive | Down Inclusive ->
            Dec

      let rel : t -> Op.Binary.Rel.t = function
        | Up Exclusive ->
            Lt
        | Up Inclusive ->
            Le
        | Down Exclusive ->
            Gt
        | Down Inclusive ->
            Ge
    end

    type t =
      { lvalue: Lvalue.t
      ; init_value: Expression.t
      ; cmp_value: Expression.t
      ; direction: Direction.t }
    [@@deriving accessors, sexp, compare, equal]
  end

  let for_loop_lvalue_unify (ilv : Lvalue.t) (clv : Lvalue.t)
      (ulv : Lvalue.t) : Lvalue.t Or_error.t =
    if not (Lvalue.equal ilv clv) then
      Or_error.error_s
        [%message
          "Init and comparison lvalues differ; not supported in FIR"
            ~init:(ilv : Lvalue.t)
            ~comparison:(clv : Lvalue.t)]
    else if not ([%equal: Lvalue.t] ilv ulv) then
      Or_error.error_s
        [%message
          "Init and update lvalues differ; not supported in FIR"
            ~init:(ilv : Lvalue.t)
            ~update:(ulv : Lvalue.t)]
    else Ok ilv

  let for_loop_direction (op : Op.Binary.Rel.t) (asn : Assign.Source.t) :
      Simple.Direction.t Or_error.t =
    (* We could map != to < and >, but this would violate the well-formedness
       property of variant accessors. *)
    match (op, asn) with
    | Lt, Inc | Ne, Inc ->
        Ok (Up Exclusive)
    | Lt, _ ->
        Or_error.error_string "if comparison op is <, update must be ++"
    | Le, Inc ->
        Ok (Up Inclusive)
    | Le, _ ->
        Or_error.error_string "if comparison op is <=, update must be ++"
    | Gt, Dec ->
        Ok (Down Exclusive)
    | Gt, _ ->
        Or_error.error_string "if comparison op is >, update must be --"
    | Ge, Dec ->
        Ok (Down Inclusive)
    | Ge, _ ->
        Or_error.error_string "if comparison op is >=, update must be --"
    | _, _ ->
        Or_error.error_s
          [%message
            "unsupported comparison/update operators"
              ~comparison:(op : Op.Binary.Rel.t)
              ~update:(asn : Assign.Source.t)]

  let unbop (cmp : Expression.t) :
      (Op.Binary.Rel.t * Lvalue.t * Expression.t) Or_error.t =
    Or_error.Let_syntax.(
      let%bind bop, l, r =
        Result.of_option
          cmp.@?(Expression.Acc.bop)
          ~error:
            (Error.create_s
               [%message
                 "Unsupported comparison expression" ~cmp:(cmp : Expression.t)])
      in
      let%bind lv =
        Result.of_option
          l.@?(Expression.Acc.address @> Address.lvalue)
          ~error:
            (Error.create_s
               [%message
                 "Unsupported LHS of comparison expression"
                   ~l:(l : Expression.t)])
      in
      match bop with
      | Rel o ->
          Ok (o, lv, r)
      | _ ->
          Or_error.error_s
            [%message
              "Unsupported comparison operator" ~op:(bop : Op.Binary.t)])

  let init_value (init : Assign.t) : Expression.t Or_error.t =
    Result.of_option
      init.@?(Assign.src @> Assign.Source.expr)
      ~error:
        (Error.create_s
           [%message
             "Unsupported RHS of initialiser expression"
               ~init:(init : Assign.t)])

  let try_simplify_inner (init : Assign.t) (cmp : Expression.t)
      (update : Assign.t) : Simple.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind cop, clv, cmp_value = unbop cmp in
      let%bind lvalue =
        for_loop_lvalue_unify init.@(Assign.dst) clv update.@(Assign.dst)
      in
      let%bind direction = for_loop_direction cop update.@(Assign.src) in
      let%map init_value = init_value init in
      {Simple.lvalue; init_value; cmp_value; direction})

  let try_simplify ({init; cmp; update} : t) : Simple.t Or_error.t =
    match (init, cmp, update) with
    | Some init, Some cmp, Some update ->
        try_simplify_inner init cmp update
    | None, _, _ ->
        Or_error.error_string "missing init in for loop"
    | Some _, None, _ ->
        Or_error.error_string "missing comparison in for loop"
    | Some _, Some _, None ->
        Or_error.error_string "missing update in for loop"

  let match_simple (x : t) : (Simple.t, t) Either.t =
    match try_simplify x with Ok s -> First s | _ -> Second x

  let construct_simple (s : Simple.t) : t =
    { init= Some Assign.(s.lvalue @= s.init_value)
    ; cmp=
        Some
          (Expression.bop
             (Rel (Simple.Direction.rel s.direction))
             (Expression.lvalue s.lvalue)
             s.cmp_value)
    ; update=
        Some
          (Assign.make ~dst:s.lvalue ~src:(Simple.Direction.src s.direction))
    }

  let simple : type i. (i, Simple.t, t, [< variant]) Accessor.Simple.t =
    [%accessor
      Accessor.variant ~match_:match_simple ~construct:construct_simple]

  (* TODO(@MattWindsor91): this should be available as an accessor, too, but
     would depend on being able to get the lvalues of expressions as such -
     either needing applicative traversals or accessors all the way down. *)
  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue
    module ELN =
      Travesty.Traversable.Chain0
        (Expression_traverse.On_addresses)
        (Address.On_lvalues)

    module On (M : Applicative.S) = struct
      module EL = ELN.On (M)
      module AL = Assign.On_lvalues.On (M)
      module O = Tx.Option.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        M.map3
          ~f:(fun init cmp update -> {init; cmp; update})
          (O.map_m ~f:(AL.map_m ~f) x.init)
          (O.map_m ~f:(EL.map_m ~f) x.cmp)
          (O.map_m ~f:(AL.map_m ~f) x.update)
    end
  end)
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

  (* Ideally we want an [lvalues_many] here, but it's stuck on the expression
     traversals all being monadic. *)

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
      with type t = t
       and type Elt.t = Expression.t = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Expression

    module On (M : Applicative.S) = struct
      module AccM = Accessor.Of_applicative (M)

      let map_m : t -> f:(Elt.t -> Elt.t M.t) -> t M.t = AccM.map exprs
    end
  end)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue
    module ELN =
      Travesty.Traversable.Chain0
        (Expression_traverse.On_addresses)
        (Address.On_lvalues)

    module On (M : Applicative.S) = struct
      module EL = ELN.On (M)
      module FL = For.On_lvalues.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        M.(
          match x with
          | For n ->
              FL.map_m ~f n >>| fun f' -> For f'
          | While (w, e) ->
              EL.map_m ~f e >>| fun e' -> While (w, e')
          | (Explicit | Implicit | Lock _) as x ->
              return x)
    end
  end)
end

type ('meta, 'stm) t = {header: Header.t; body: ('meta, 'stm) Block.t}
[@@deriving sexp, make, accessors, compare, equal]

let for_loop (body : ('meta, 'stm) Block.t) ~(control : For.t) :
    ('meta, 'stm) t =
  make ~body ~header:(Header.For control)

let for_loop_simple (body : ('meta, 'stm) Block.t) ~(control : For.Simple.t)
    : ('meta, 'stm) t =
  make ~body ~header:(Accessor.construct (Header.for_ @> For.simple) control)

let while_loop ~(cond : Expression.t) ~(body : ('meta, 'stm) Block.t)
    ~(kind : While.t) : ('meta, 'stm) t =
  make ~body ~header:(Header.While (kind, cond))

let lock_block (body : ('meta, 'stm) Block.t) ~(kind : Lock.t) :
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

module Bident = Base_map (Travesty.Monad_exts.App (Monad.Ident))

let map = Bident.bmap
