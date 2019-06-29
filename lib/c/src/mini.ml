(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel
module Tx = Travesty_base_exts
module Ac = Act_common
module Constant = Act_c_lang.Ast_basic.Constant
module Identifier = Act_c_lang.Ast_basic.Identifier
module Pointer = Act_c_lang.Ast_basic.Pointer
module Assign = Mini_assign
module Address = Mini_address
module Env = Mini_env
module Expression = Mini_expression
module Initialiser = Mini_initialiser
module Lvalue = Mini_lvalue
module Type = Mini_type
module Atomic_load = Expression.Atomic_load
open Mini_intf

module Atomic_store = struct
  type t = {src: Expression.t; dst: Address.t; mo: Mem_order.t}
  [@@deriving sexp, fields, make]

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (store : t) ~(src : Expression.t F.traversal)
        ~(dst : Address.t F.traversal) ~(mo : Mem_order.t F.traversal) :
        t M.t =
      Fields.fold ~init:(M.return store) ~src:(F.proc_field src)
        ~dst:(F.proc_field dst) ~mo:(F.proc_field mo)
  end

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module A = Address.On_lvalues.On_monad (M)

      let map_m x ~f =
        B.bmap x ~src:(E.map_m ~f) ~dst:(A.map_m ~f) ~mo:M.return
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Address

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_addresses.On_monad (M)

      let map_m x ~f = B.bmap x ~src:(E.map_m ~f) ~dst:f ~mo:M.return
    end
  end)

  module Quickcheck_generic
      (Src : Quickcheck.S with type t := Expression.t)
      (Dst : Quickcheck.S with type t := Address.t) :
    Act_utils.My_quickcheck.S_with_sexp with type t = t = struct
    type nonrec t = t

    let sexp_of_t = sexp_of_t

    let to_tuple ({src; dst; mo} : t) :
        Expression.t * Address.t * Mem_order.t =
      (src, dst, mo)

    let of_tuple ((src, dst, mo) : Expression.t * Address.t * Mem_order.t) :
        t =
      {src; dst; mo}

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.(
        map
          [%quickcheck.generator:
            Src.t * Dst.t * [%custom Mem_order.gen_store]] ~f:of_tuple)

    let quickcheck_observer : t Base_quickcheck.Observer.t =
      Base_quickcheck.Observer.(
        unmap [%quickcheck.observer: Src.t * Dst.t * Mem_order.t]
          ~f:to_tuple)

    let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
      Base_quickcheck.Shrinker.(
        map [%quickcheck.shrinker: Src.t * Dst.t * Mem_order.t] ~f:of_tuple
          ~f_inverse:to_tuple)
  end

  module Quickcheck_ints (Src : Env.S) (Dst : Env.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t = t =
    Quickcheck_generic
      (Expression.Quickcheck_int_values
         (Src))
         (Address.Quickcheck_atomic_int_pointers (Dst))
end

module Atomic_cmpxchg = struct
  type t =
    { obj: Address.t
    ; expected: Address.t
    ; desired: Expression.t
    ; succ: Mem_order.t
    ; fail: Mem_order.t }
  [@@deriving sexp, fields, make]

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (cmpxchg : t) ~(obj : Address.t F.traversal)
        ~(expected : Address.t F.traversal)
        ~(desired : Expression.t F.traversal)
        ~(succ : Mem_order.t F.traversal) ~(fail : Mem_order.t F.traversal)
        : t M.t =
      Fields.fold ~init:(M.return cmpxchg) ~obj:(F.proc_field obj)
        ~expected:(F.proc_field expected) ~desired:(F.proc_field desired)
        ~succ:(F.proc_field succ) ~fail:(F.proc_field fail)
  end

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module A = Address.On_lvalues.On_monad (M)

      let map_m x ~f =
        B.bmap x ~obj:(A.map_m ~f) ~expected:(A.map_m ~f)
          ~desired:(E.map_m ~f) ~succ:M.return ~fail:M.return
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Address

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_addresses.On_monad (M)

      let map_m x ~f =
        B.bmap x ~obj:f ~expected:f ~desired:(E.map_m ~f) ~succ:M.return
          ~fail:M.return
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
  {cond: Expression.t; t_branch: statement list; f_branch: statement list}
[@@deriving sexp, fields]

module Ifs_base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)
  module O = Tx.Option.On_monad (M)

  let bmap (if_stm : if_statement) ~(cond : Expression.t F.traversal)
      ~(t_branch : statement list F.traversal)
      ~(f_branch : statement list F.traversal) : if_statement M.t =
    Fields_of_if_statement.fold ~init:(M.return if_stm)
      ~cond:(F.proc_field cond) ~t_branch:(F.proc_field t_branch)
      ~f_branch:(F.proc_field f_branch)
end

module Statement :
  S_statement
    with type address := Address.t
     and type assign := Assign.t
     and type atomic_cmpxchg := Atomic_cmpxchg.t
     and type atomic_store := Atomic_store.t
     and type identifier := Identifier.t
     and type if_stm := if_statement
     and type t = statement
     and type lvalue := Lvalue.t = struct
  type t = statement [@@deriving sexp]

  let assign = P_statement.assign

  let atomic_cmpxchg = P_statement.atomic_cmpxchg

  let atomic_store = P_statement.atomic_store

  let if_stm = P_statement.if_stm

  let nop () = P_statement.nop

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
      P_statement.Variants.map x ~assign:(F.proc_variant1 assign)
        ~atomic_cmpxchg:(F.proc_variant1 atomic_cmpxchg)
        ~atomic_store:(F.proc_variant1 atomic_store)
        ~if_stm:(F.proc_variant1 if_stm) ~nop:(F.proc_variant0 nop)
  end

  let map x ~assign ~atomic_cmpxchg ~atomic_store ~if_stm ~nop =
    P_statement.Variants.map x ~assign:(Fn.const assign)
      ~atomic_store:(Fn.const atomic_store)
      ~atomic_cmpxchg:(Fn.const atomic_cmpxchg) ~if_stm:(Fn.const if_stm)
      ~nop:(fun _ -> nop ())

  (* We have to unroll the map over if statements here, because otherwise we
     end up with unsafe module recursion. *)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module L = Tx.List.On_monad (M)
      module Asn = Assign.On_lvalues.On_monad (M)
      module Sto = Atomic_store.On_lvalues.On_monad (M)
      module Cxg = Atomic_cmpxchg.On_lvalues.On_monad (M)

      let rec map_m x ~f =
        B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
          ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
          ~nop:M.return

      and map_m_ifs x ~f =
        IB.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(map_m ~f))
          ~f_branch:(L.map_m ~f:(map_m ~f))
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Address

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module IB = Ifs_base_map (M)
      module E = Expression.On_addresses.On_monad (M)
      module L = Tx.List.On_monad (M)
      module Asn = Assign.On_addresses.On_monad (M)
      module Sto = Atomic_store.On_addresses.On_monad (M)
      module Cxg = Atomic_cmpxchg.On_addresses.On_monad (M)

      let rec map_m x ~f =
        B.bmap x ~assign:(Asn.map_m ~f) ~atomic_store:(Sto.map_m ~f)
          ~atomic_cmpxchg:(Cxg.map_m ~f) ~if_stm:(map_m_ifs ~f)
          ~nop:M.return

      and map_m_ifs x ~f =
        IB.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(map_m ~f))
          ~f_branch:(L.map_m ~f:(map_m ~f))
    end
  end)

  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Identifier.t =
    Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)
end

module If_statement :
  S_if_statement
    with type expr := Expression.t
     and type stm := Statement.t
     and type t = if_statement
     and type address := Address.t
     and type identifier := Identifier.t
     and type lvalue := Lvalue.t = struct
  type t = if_statement [@@deriving sexp]

  let cond = Field.get Fields_of_if_statement.cond

  let f_branch = Field.get Fields_of_if_statement.f_branch

  let t_branch = Field.get Fields_of_if_statement.t_branch

  let make ~cond ?(t_branch = []) ?(f_branch = []) () =
    Fields_of_if_statement.create ~cond ~t_branch ~f_branch

  module Base_map (M : Monad.S) = Ifs_base_map (M)

  module On_lvalues :
    Travesty.Traversable_types.S0 with type t := t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Lvalue

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_lvalues.On_monad (M)
      module S = Statement.On_lvalues.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m x ~f =
        B.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(S.map_m ~f))
          ~f_branch:(L.map_m ~f:(S.map_m ~f))
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0
      with type t := t
       and type Elt.t = Address.t = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Address

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module E = Expression.On_addresses.On_monad (M)
      module S = Statement.On_addresses.On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m x ~f =
        B.bmap x ~cond:(E.map_m ~f)
          ~t_branch:(L.map_m ~f:(S.map_m ~f))
          ~f_branch:(L.map_m ~f:(S.map_m ~f))
    end
  end)

  module On_identifiers :
    Travesty.Traversable_types.S0
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
    { parameters: Type.t id_assoc
    ; body_decls: Initialiser.t id_assoc
    ; body_stms: Statement.t list }
  [@@deriving sexp, fields, make]

  let with_body_stms (func : t) (new_stms : Statement.t list) : t =
    {func with body_stms= new_stms}

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let map_m (func : t)
        ~(parameters : Type.t id_assoc -> Type.t id_assoc M.t)
        ~(body_decls : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(body_stms : Statement.t list -> Statement.t list M.t) : t M.t =
      Fields.fold ~init:(M.return func)
        ~parameters:(F.proc_field parameters)
        ~body_decls:(F.proc_field body_decls)
        ~body_stms:(F.proc_field body_stms)
  end

  let map =
    let module M = On_monad (Monad.Ident) in
    M.map_m

  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.Named.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Initialiser.Named

    module On_monad (M : Monad.S) = struct
      module B = On_monad (M)
      module L = Tx.List.On_monad (M)

      let map_m (func : t)
          ~(f : Initialiser.Named.t -> Initialiser.Named.t M.t) =
        B.map_m func ~parameters:M.return ~body_decls:(L.map_m ~f)
          ~body_stms:M.return
    end
  end)

  let cvars (func : t) : Ac.C_id.Set.t =
    func |> On_decls.to_list |> List.map ~f:fst |> Ac.C_id.Set.of_list
end

module Program = struct
  type t = {globals: Initialiser.t id_assoc; functions: Function.t id_assoc}
  [@@deriving sexp, fields, make]

  let with_functions (program : t) (new_functions : Function.t id_assoc) : t
      =
    {program with functions= new_functions}

  module Base_map (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let bmap (program : t)
        ~(globals : Initialiser.t id_assoc -> Initialiser.t id_assoc M.t)
        ~(functions : Function.t id_assoc -> Function.t id_assoc M.t) :
        t M.t =
      Fields.fold ~init:(M.return program) ~globals:(F.proc_field globals)
        ~functions:(F.proc_field functions)
  end

  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.Named.t =
  Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Initialiser.Named

    module On_monad (M : Monad.S) = struct
      module B = Base_map (M)
      module L = Tx.List.On_monad (M)
      module F = Function.On_decls.On_monad (M)

      let map_m (program : t) ~(f : Elt.t -> Elt.t M.t) =
        B.bmap program ~globals:(L.map_m ~f)
          ~functions:
            (L.map_m ~f:(fun (k, v) -> M.(F.map_m ~f v >>| Tuple2.create k)))
    end
  end)

  let cvars (prog : t) : Ac.C_id.Set.t =
    prog |> On_decls.to_list |> List.map ~f:fst |> Ac.C_id.Set.of_list
end
