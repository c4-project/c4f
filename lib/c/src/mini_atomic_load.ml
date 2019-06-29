(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Address = Mini_address
module Env = Mini_env
module Lvalue = Mini_lvalue
module Type = Mini_type

type t = {src: Address.t; mo: Mem_order.t} [@@deriving sexp, fields, make]

let to_tuple ({src; mo} : t) : Address.t * Mem_order.t = (src, mo)

let of_tuple ((src, mo) : Address.t * Mem_order.t) : t = {src; mo}

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (store : t) ~(src : Address.t F.traversal)
      ~(mo : Mem_order.t F.traversal) : t M.t =
    Fields.fold ~init:(M.return store) ~src:(F.proc_field src)
      ~mo:(F.proc_field mo)
end

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)

    let map_m x ~f = B.bmap x ~src:f ~mo:M.return
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
  Travesty.Traversable.Chain0 (On_addresses) (Address.On_lvalues)

module Type_check (E : Env.S) = struct
  module A = Address.Type_check (E)

  let type_of (ld : t) : Type.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind a_ptr = A.type_of (src ld) in
    let%bind a = Type.deref a_ptr in
    Type.to_non_atomic a
end

let%expect_test "type_of: atomic_int* -> int" =
  let (module E) = Lazy.force Env.test_env_mod in
  let module Ty = Type_check (E) in
  let src = Address.lvalue (Lvalue.variable (Ac.C_id.of_string "bar")) in
  let ld = make ~src ~mo:Mem_order.Seq_cst in
  Stdio.print_s [%sexp (Ty.type_of ld : Type.t Or_error.t)] ;
  [%expect {| (Ok (Normal int)) |}]

module Quickcheck_generic
    (A : Act_utils.My_quickcheck.S_with_sexp with type t := Address.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map ~f:of_tuple
      [%quickcheck.generator: A.t * [%custom Mem_order.gen_load]]

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    Base_quickcheck.Observer.unmap ~f:to_tuple
      [%quickcheck.observer: A.t * Mem_order.t]

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.map ~f:of_tuple ~f_inverse:to_tuple
      [%quickcheck.shrinker: A.t * Mem_order.t]
end

module Quickcheck_main = Quickcheck_generic (Address)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

module Quickcheck_atomic_ints (E : Env.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end =
  Quickcheck_generic (Address.Quickcheck_atomic_int_pointers (E))

let variable_of (ld : t) : Ac.C_id.t = Address.variable_of (src ld)

let variable_in_env (ld : t) ~(env : _ Ac.C_id.Map.t) : bool =
  Address.variable_in_env (src ld) ~env

let%test_unit "Quickcheck_atomic_ints: liveness" =
  let (module E) = Lazy.force Env.test_env_mod in
  let module Q = Quickcheck_atomic_ints (E) in
  Core_kernel.Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t]
    ~f:(variable_in_env ~env:E.env)

let%test_unit "Quickcheck_atomic_ints: generated underlying variables in \
               environment" =
  let (module E) = Lazy.force Env.test_env_mod in
  let module Q = Quickcheck_atomic_ints (E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in_env ~env:E.env))
