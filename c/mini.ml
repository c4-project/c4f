(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

include Ast_basic
include Mini_intf

module Env = Mini_env
module Type = Mini_type
module Initialiser = Mini_initialiser
module Lvalue = Mini_lvalue
module Address = Mini_address

module Atomic_load = struct
  type t =
    { src : Address.t
    ; mo  : Mem_order.t
    }
  [@@deriving sexp, fields, make]
  ;;

  let to_tuple ({ src; mo } : t) : Address.t * Mem_order.t = ( src, mo )
  let of_tuple (( src, mo ) : Address.t * Mem_order.t) : t = { src; mo }

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (store : t)
        ~(src : Address.t    F.traversal)
        ~(mo  : Mem_order.t  F.traversal)
      : t M.t =
      Fields.fold
        ~init:(M.return store)
        ~src:(F.proc_field src)
        ~mo:(F.proc_field mo)
    ;;
  end

  module On_addresses : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        let map_m x ~f = B.bmap x ~src:f ~mo:(M.return)
      end
    end)

  module On_lvalues : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_addresses
      end)
      (Address.On_lvalues)
  ;;

  module Type_check (E : Env.S) = struct
    module A = Address.Type_check (E)

    let type_of (ld : t) : Type.t Or_error.t =
      let open Or_error.Let_syntax in
      let%bind a_ptr = A.type_of (src ld) in
      let%bind a     = Type.deref a_ptr in
      Type.to_non_atomic a
    ;;
  end

  let%expect_test "type_of: atomic_int* -> int" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Ty  = Type_check (E) in
    let src =
      Address.lvalue (Lvalue.variable (C_identifier.of_string "bar"))
    in
    let ld =  make ~src ~mo:Mem_order.Seq_cst in
    Sexp.output_hum stdout [%sexp (Ty.type_of ld : Type.t Or_error.t)];
    [%expect {| (Ok (Normal int)) |}]
  ;;

  module Quickcheck_general (A : Quickcheckable.S with type t := Address.t)
    : Quickcheckable.S with type t := t = struct
    module Gen = Core_kernel.Quickcheck.Generator
    module Obs = Core_kernel.Quickcheck.Observer
    module Snk = Core_kernel.Quickcheck.Shrinker

    let gen : t Gen.t =
      Gen.(map (tuple2 A.gen Mem_order.gen_load) ~f:of_tuple)
    ;;

    let obs : t Obs.t =
      Obs.(unmap (tuple2 A.obs Mem_order.obs) ~f:to_tuple)
    ;;

    let shrinker : t Snk.t =
      Snk.(map (tuple2 A.shrinker Mem_order.shrinker)
             ~f:of_tuple ~f_inverse:to_tuple
          )
    ;;
  end
  include Quickcheck_general (Address)

  module Quickcheck_atomic_ints (E : Env.S)
    : Quickcheckable.S with type t := t =
    Quickcheck_general (Address.Quickcheck_atomic_int_pointers (E))
  ;;

  let variable_of (ld : t) : C_identifier.t =
    Address.variable_of (src ld)
  ;;

  let variable_in_env (ld : t) ~(env : _ C_identifier.Map.t) : bool =
    Address.variable_in_env (src ld) ~env
  ;;

  let%test_unit
    "Quickcheck_atomic_ints: liveness" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Q = Quickcheck_atomic_ints (E) in
    Quickcheck.test_can_generate Q.gen
      ~sexp_of:[%sexp_of: t]
      ~f:(variable_in_env ~env:E.env)
  ;;

  let%test_unit
    "Quickcheck_atomic_ints: generated underlying variables in environment" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Q = Quickcheck_atomic_ints (E) in
    Quickcheck.test Q.gen
      ~sexp_of:[%sexp_of: t]
      ~shrinker:Q.shrinker
      ~f:([%test_pred: t] ~here:[[%here]] (variable_in_env ~env:E.env))
  ;;
end

module Expression = struct
  type t =
    | Constant    of Constant.t
    | Lvalue      of Lvalue.t
    | Atomic_load of Atomic_load.t
    | Eq          of t * t
  [@@deriving sexp, variants]
  ;;

  let reduce (expr : t)
    ~(constant    : Constant.t -> 'a)
    ~(lvalue      : Lvalue.t   -> 'a)
    ~(atomic_load : Atomic_load.t -> 'a)
    ~(eq          : 'a -> 'a   -> 'a) : 'a =
    let rec mu = function
      | Constant    k      -> constant k
      | Lvalue      l      -> lvalue l
      | Atomic_load ld     -> atomic_load ld
      | Eq          (x, y) -> eq (mu x) (mu y)
    in mu expr
  ;;

  let anonymise = function
    | Constant    k      -> `A k
    | Lvalue      l      -> `B l
    | Eq          (x, y) -> `C ((x, y))
    | Atomic_load ld     -> `D ld
  ;;

  module On_addresses
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module F = Travesty.Traversable.Helpers (M)
        module A = Atomic_load.On_addresses.On_monad (M)

        let rec map_m x ~f =
          Variants.map x
            ~constant:(F.proc_variant1 M.return)
            ~lvalue:(F.proc_variant1 M.return)
            ~eq:(F.proc_variant2
                   (fun (l, r) ->
                      let open M.Let_syntax in
                      let%bind l' = map_m l ~f in
                      let%map  r' = map_m r ~f in
                      (l', r')))
            ~atomic_load:(F.proc_variant1 (A.map_m ~f))
      end
    end)

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module A = Atomic_load.On_lvalues.On_monad (M)
        module F = Travesty.Traversable.Helpers (M)

        let rec map_m x ~f =
          Variants.map x
            ~constant:(F.proc_variant1 M.return)
            ~lvalue:(F.proc_variant1 f)
            ~eq:(F.proc_variant2
                   (fun (l, r) ->
                      let open M.Let_syntax in
                      let%bind l' = map_m l ~f in
                      let%map  r' = map_m r ~f in
                      (l', r')))
            ~atomic_load:(F.proc_variant1 (A.map_m ~f))
      end
    end)

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_lvalues
      end)
      (Lvalue.On_identifiers)

  module Type_check (E : Env.S) = struct
    module Lv = Lvalue.Type_check (E)
    module Ld = Atomic_load.Type_check (E)

    let type_of_constant : Constant.t -> Type.t Or_error.t = function
      | Char    _ -> Or_error.unimplemented "char type"
      | Float   _ -> Or_error.unimplemented "float type"
      | Integer _ -> Or_error.return Type.(normal Basic.int)
    ;;

    let rec type_of : t -> Type.t Or_error.t = function
      | Constant    k      -> type_of_constant k
      | Lvalue      l      -> Lv.type_of l
      | Eq          (l, r) -> type_of_relational l r
      | Atomic_load ld     -> Ld.type_of ld
    and type_of_relational (l : t) (r : t) : Type.t Or_error.t =
      let open Or_error.Let_syntax in
      let%map _ = type_of l
      and     _ = type_of r
      in Type.(normal Basic.bool)
    ;;
  end

  module Quickcheck_int_values (E : Env.S)
    : Quickcheckable.S with type t := t = struct
    module Gen = Quickcheck.Generator
    module Obs = Quickcheck.Observer
    module Snk = Quickcheck.Shrinker

    module L = Lvalue.Quickcheck_int_values (E)
    module A = Atomic_load.Quickcheck_atomic_ints (E)

    (** Generates the terminal integer expressions. *)
    let base_generators : t Gen.t list =
      [ Gen.map ~f:constant Constant.gen_int32_constant
      ; Gen.map ~f:atomic_load A.gen
      ; Gen.map ~f:lvalue L.gen
      ]

    let recursive_generators (_mu : t Gen.t) : t Gen.t list =
      [] (* No useful recursive expression types yet. *)
    ;;

    let gen : t Gen.t =
      Gen.recursive_union base_generators ~f:recursive_generators
    ;;

    let obs : t Obs.t =
      Quickcheck.Observer.fixed_point
        (fun mu ->
           Obs.unmap ~f:anonymise
             (Obs.variant4
                Constant.obs
                Lvalue.obs
                (Obs.tuple2 mu mu)
                Atomic_load.obs
             )
        )
    ;;

    (* TODO(@MattWindsor91): implement this *)
    let shrinker : t Snk.t = Snk.empty ()
  end

  let%test_unit
    "Quickcheck_int_values: liveness" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Ty = Type_check (E) in
    let module Q = Quickcheck_int_values (E) in
    Quickcheck.test_can_generate Q.gen
      ~sexp_of:[%sexp_of: t]
      ~f:(fun e ->
          Type.([%compare.equal: t Or_error.t]
                  (Ty.type_of e)
                  (Or_error.return (normal Basic.int))
               )
        )
  ;;

  let%test_unit
    "Quickcheck_int_values: all expressions have 'int' type" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Ty = Type_check (E) in
    let module Q = Quickcheck_int_values (E) in
    Quickcheck.test Q.gen
      ~sexp_of:[%sexp_of: t]
      ~shrinker:Q.shrinker
      ~f:(fun e ->
          [%test_result: Type.t Or_error.t]
            (Ty.type_of e)
            ~here:[[%here]]
            ~equal:[%compare.equal: Type.t Or_error.t]
            ~expect:(Or_error.return Type.(normal Basic.int))
        )
  ;;

  let%test_unit
    "Quickcheck_int_values: all referenced variables in environment" =
    let (module E) = Lazy.force Env.test_env_mod in
    let module Q = Quickcheck_int_values (E) in
    Quickcheck.test Q.gen
      ~sexp_of:[%sexp_of: t]
      ~shrinker:Q.shrinker
      ~f:([%test_pred: t]
         (On_identifiers.for_all ~f:(C_identifier.Map.mem E.env))
         ~here:[[%here]]
         )
  ;;
(*
  module Quickcheck_bools (E : Env.S)
    : Quickcheckable.S with type t := t = struct
    module G = Quickcheck.Generator
    module O = Quickcheck.Observer
    module S = Quickcheck.Shrinker

    let gen : t G.t =
      let open G.Let_syntax in
      Quickcheck.Generator.union
        [ gen_int_relational
        ; gen_const
        ]
  end *)
end

module Assign = struct
  type t =
    { lvalue : Lvalue.t
    ; rvalue : Expression.t
    }
  [@@deriving sexp, fields, make]
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (assign : t)
        ~(lvalue : Lvalue.t F.traversal)
        ~(rvalue : Expression.t F.traversal)
      : t M.t =
      Fields.fold
        ~init:(M.return assign)
        ~lvalue:(F.proc_field lvalue)
        ~rvalue:(F.proc_field rvalue)
    ;;
  end

  module On_lvalues : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_lvalues.On_monad (M)

        let map_m x ~f = B.bmap x ~lvalue:f ~rvalue:(E.map_m ~f)
      end
    end)

  module On_addresses : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)

        let map_m x ~f = B.bmap x ~lvalue:M.return ~rvalue:(E.map_m ~f)
      end
    end)
end

module Atomic_store = struct
  type t =
    { src : Expression.t
    ; dst : Address.t
    ; mo  : Mem_order.t
    }
  [@@deriving sexp, fields, make]
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (store : t)
        ~(src : Expression.t F.traversal)
        ~(dst : Address.t    F.traversal)
        ~(mo  : Mem_order.t  F.traversal)
      : t M.t =
      Fields.fold
        ~init:(M.return store)
        ~src:(F.proc_field src)
        ~dst:(F.proc_field dst)
        ~mo:(F.proc_field mo)
    ;;
  end

  module On_lvalues : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_lvalues.On_monad (M)
        module A = Address.On_lvalues.On_monad (M)

        let map_m x ~f =
          B.bmap x ~src:(E.map_m ~f) ~dst:(A.map_m ~f) ~mo:(M.return)
      end
    end)

  module On_addresses : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)

        let map_m x ~f =
          B.bmap x ~src:(E.map_m ~f) ~dst:f ~mo:(M.return)
      end
    end)
end

module Atomic_cmpxchg = struct
  type t =
    { obj      : Address.t
    ; expected : Address.t
    ; desired  : Expression.t
    ; succ     : Mem_order.t
    ; fail     : Mem_order.t
    }
  [@@deriving sexp, fields, make]

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (cmpxchg : t)
        ~(obj      : Address.t    F.traversal)
        ~(expected : Address.t    F.traversal)
        ~(desired  : Expression.t F.traversal)
        ~(succ     : Mem_order.t  F.traversal)
        ~(fail     : Mem_order.t  F.traversal)
      : t M.t =
      Fields.fold
        ~init:(M.return cmpxchg)
        ~obj:(F.proc_field obj)
        ~expected:(F.proc_field expected)
        ~desired:(F.proc_field desired)
        ~succ:(F.proc_field succ)
        ~fail:(F.proc_field fail)
    ;;
  end

  module On_lvalues : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_lvalues.On_monad (M)
        module A = Address.On_lvalues.On_monad (M)

        let map_m x ~f =
          B.bmap x
            ~obj:(A.map_m ~f) ~expected:(A.map_m ~f)
            ~desired:(E.map_m ~f)
            ~succ:(M.return) ~fail:(M.return)
      end
    end)

  module On_addresses : Travesty.Traversable.S0_container
    with type t := t and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)

        let map_m x ~f =
          B.bmap x
            ~obj:f ~expected:f
            ~desired:(E.map_m ~f)
            ~succ:(M.return) ~fail:(M.return)
      end
    end)
end

module P_statement = struct
  type 'i t =
    | Assign of Assign.t
    | Atomic_store of Atomic_store.t
    | Atomic_cmpxchg of Atomic_cmpxchg.t
    | Nop
    | If_stm of 'i
  [@@deriving sexp, variants]
end

type statement = if_statement P_statement.t
and if_statement =
    { cond     : Expression.t
    ; t_branch : statement list
    ; f_branch : statement list
    }
  [@@deriving sexp, fields]
;;

module Ifs_base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)
  module O = Travesty.T_option.On_monad (M)

  let bmap (if_stm : if_statement)
      ~(cond     : Expression.t F.traversal)
      ~(t_branch : statement list   F.traversal)
      ~(f_branch : statement list   F.traversal)
    : if_statement M.t =
    Fields_of_if_statement.fold
      ~init:(M.return if_stm)
      ~cond:(F.proc_field cond)
      ~t_branch:(F.proc_field t_branch)
      ~f_branch:(F.proc_field f_branch)
end

module Statement
  : (S_statement with type address        := Address.t
                  and type assign         := Assign.t
                  and type atomic_cmpxchg := Atomic_cmpxchg.t
                  and type atomic_store   := Atomic_store.t
                  and type identifier     := Identifier.t
                  and type if_stm         := if_statement
                  and type t               = statement
                  and type lvalue         := Lvalue.t)
= struct
  type t = statement [@@deriving sexp]
  let assign = P_statement.assign
  let atomic_cmpxchg = P_statement.atomic_cmpxchg
  let atomic_store = P_statement.atomic_store
  let if_stm = P_statement.if_stm
  let nop () = P_statement.nop

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
      P_statement.Variants.map x
        ~assign:(F.proc_variant1 assign)
        ~atomic_cmpxchg:(F.proc_variant1 atomic_cmpxchg)
        ~atomic_store:(F.proc_variant1 atomic_store)
        ~if_stm:(F.proc_variant1 if_stm)
        ~nop:(F.proc_variant0 nop)
    ;;
  end

  let map x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
    P_statement.Variants.map x
      ~assign:(Fn.const assign)
      ~atomic_store:(Fn.const atomic_store)
      ~atomic_cmpxchg:(Fn.const atomic_cmpxchg)
      ~if_stm:(Fn.const if_stm)
      ~nop:(fun _ -> nop ())
  ;;

(* We have to unroll the map over if statements here, because
   otherwise we end up with unsafe module recursion. *)

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B   = Base_map                           (M)
        module IB  = Ifs_base_map                       (M)
        module E   = Expression.On_lvalues.On_monad     (M)
        module L   = Travesty.T_list.On_monad           (M)
        module Asn = Assign.On_lvalues.On_monad         (M)
        module Sto = Atomic_store.On_lvalues.On_monad   (M)
        module Cxg = Atomic_cmpxchg.On_lvalues.On_monad (M)

        let rec map_m x ~f =
          B.bmap x
            ~assign:(Asn.map_m ~f)
            ~atomic_store:(Sto.map_m ~f)
            ~atomic_cmpxchg:(Cxg.map_m ~f)
            ~if_stm:(map_m_ifs ~f)
            ~nop:M.return
        and map_m_ifs x ~f =
          IB.bmap x
            ~cond:(E.map_m ~f)
            ~t_branch:(L.map_m ~f:(map_m ~f))
            ~f_branch:(L.map_m ~f:(map_m ~f))
        ;;
      end
    end)

  module On_addresses
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map                               (M)
        module IB  = Ifs_base_map                         (M)
        module E = Expression.On_addresses.On_monad       (M)
        module L = Travesty.T_list.On_monad               (M)
        module Asn = Assign.On_addresses.On_monad         (M)
        module Sto = Atomic_store.On_addresses.On_monad   (M)
        module Cxg = Atomic_cmpxchg.On_addresses.On_monad (M)

        let rec map_m x ~f =
          B.bmap x
            ~assign:(Asn.map_m ~f)
            ~atomic_store:(Sto.map_m ~f)
            ~atomic_cmpxchg:(Cxg.map_m ~f)
            ~if_stm:(map_m_ifs ~f)
            ~nop:M.return
        and map_m_ifs x ~f =
          IB.bmap x
            ~cond:(E.map_m ~f)
            ~t_branch:(L.map_m ~f:(map_m ~f))
            ~f_branch:(L.map_m ~f:(map_m ~f))
        ;;
      end
    end)

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_lvalues
      end)
      (Lvalue.On_identifiers)
end

module If_statement
  : (S_if_statement with type expr       := Expression.t
                     and type stm        := Statement.t
                     and type t          =  if_statement
                     and type address    := Address.t
                     and type identifier := Identifier.t
                     and type lvalue     := Lvalue.t
    )
= struct
  type t = if_statement [@@deriving sexp]

  let cond     = Field.get Fields_of_if_statement.cond
  let f_branch = Field.get Fields_of_if_statement.f_branch
  let t_branch = Field.get Fields_of_if_statement.t_branch
  let make ~cond ?(t_branch = []) ?(f_branch = []) () =
    Fields_of_if_statement.create ~cond ~t_branch ~f_branch

  module Base_map (M : Monad.S) = Ifs_base_map (M)

  module On_lvalues
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Lvalue.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Lvalue

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_lvalues.On_monad (M)
        module S = Statement.On_lvalues.On_monad (M)
        module L = Travesty.T_list.On_monad (M)
        let map_m x ~f =
          B.bmap x
            ~cond:(E.map_m ~f)
            ~t_branch:(L.map_m ~f:(S.map_m ~f))
            ~f_branch:(L.map_m ~f:(S.map_m ~f))
      end
    end)

  module On_addresses
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Address.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Address

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module E = Expression.On_addresses.On_monad (M)
        module S = Statement.On_addresses.On_monad (M)
        module L = Travesty.T_list.On_monad (M)
        let map_m x ~f =
          B.bmap x
            ~cond:(E.map_m ~f)
            ~t_branch:(L.map_m ~f:(S.map_m ~f))
            ~f_branch:(L.map_m ~f:(S.map_m ~f))
      end
    end)

  module On_identifiers
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0
      (struct
        type nonrec t = t
        include On_lvalues
      end)
      (Lvalue.On_identifiers)
end

module Function = struct
  type t =
    { parameters : Type.t id_assoc
    ; body_decls : Initialiser.t id_assoc
    ; body_stms  : Statement.t list
    }
  [@@deriving sexp, fields, make]
  ;;

  let with_body_stms (func : t) (new_stms : Statement.t list) : t =
    { func with body_stms = new_stms }

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (func : t)
        ~(parameters : Type.t id_assoc -> Type.t id_assoc M.t)
        ~(body_decls : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(body_stms  : Statement.t list -> Statement.t list M.t)
      : t M.t =
      Fields.fold
        ~init:(M.return func)
        ~parameters:(F.proc_field parameters)
        ~body_decls:(F.proc_field body_decls)
        ~body_stms:(F.proc_field body_stms)
    ;;
  end

  let map = let module M = Base_map (Monad.Ident) in M.bmap

  module On_decls
    : Travesty.Traversable.S0_container with type t := t
                                         and type Elt.t = Initialiser.Named.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Initialiser.Named

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module L = Travesty.T_list.On_monad (M)

        let map_m
            (func : t)
            ~(f : Initialiser.Named.t -> Initialiser.Named.t M.t) =
          B.bmap func
            ~parameters:M.return
            ~body_decls:(L.map_m ~f)
            ~body_stms:M.return
      end
    end)
  ;;

  let cvars (func : t) : C_identifier.Set.t =
    func
    |> On_decls.to_list
    |> List.map ~f:fst
    |> C_identifier.Set.of_list
  ;;
end

module Program = struct
  type t =
    { globals   : Initialiser.t id_assoc
    ; functions : Function.t id_assoc
    }
  [@@deriving sexp, fields, make]
  ;;

  let with_functions
      (program : t) (new_functions : Function.t id_assoc) : t =
    { program with functions = new_functions }
  ;;

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    let bmap (program : t)
        ~(globals   : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(functions : Function.t id_assoc -> Function.t id_assoc M.t)
      : t M.t =
      Fields.fold
        ~init:(M.return program)
        ~globals:(F.proc_field globals)
        ~functions:(F.proc_field functions)
    ;;
  end

  module On_decls
    : Travesty.Traversable.S0_container
      with type t := t
       and type Elt.t := Initialiser.Named.t =
    Travesty.Traversable.Make_container0 (struct
      type nonrec t = t
      module Elt = Initialiser.Named

      module On_monad (M : Monad.S) = struct
        module B = Base_map (M)
        module L = Travesty.T_list.On_monad (M)
        module F = Function.On_decls.On_monad (M)

        let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
          B.bmap program
            ~globals:(L.map_m ~f)
            ~functions:(L.map_m ~f:(fun (k, v) -> M.(F.map_m ~f v >>| Tuple2.create k)))
        ;;
      end
    end)

  let cvars (prog : t) : C_identifier.Set.t =
    prog
    |> On_decls.to_list
    |> List.map ~f:(fst)
    |> C_identifier.Set.of_list
  ;;
end

