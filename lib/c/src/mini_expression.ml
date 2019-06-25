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
module Ac = Act_common
module Address = Mini_address
module Constant = Act_c_lang.Ast_basic.Constant
module Env = Mini_env
module Identifier = Act_c_lang.Ast_basic.Identifier
module Lvalue = Mini_lvalue
module Type = Mini_type

module Atomic_load = struct
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
end

type t =
  | Bool_lit of bool
  | Constant of Constant.t
  | Lvalue of Lvalue.t
  | Atomic_load of Atomic_load.t
  | Eq of t * t
[@@deriving sexp, variants]

let reduce (expr : t) ~(bool_lit : bool -> 'a)
    ~(constant : Constant.t -> 'a) ~(lvalue : Lvalue.t -> 'a)
    ~(atomic_load : Atomic_load.t -> 'a) ~(eq : 'a -> 'a -> 'a) : 'a =
  let rec mu = function
    | Bool_lit b ->
        bool_lit b
    | Constant k ->
        constant k
    | Lvalue l ->
        lvalue l
    | Atomic_load ld ->
        atomic_load ld
    | Eq (x, y) ->
        eq (mu x) (mu y)
  in
  mu expr

let anonymise = function
  | Bool_lit blit ->
      `A blit
  | Constant k ->
      `B k
  | Lvalue l ->
      `C l
  | Eq (x, y) ->
      `D (x, y)
  | Atomic_load ld ->
      `E ld

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    module A = Atomic_load.On_addresses.On_monad (M)

    let rec map_m x ~f =
      Variants.map x
        ~bool_lit:(F.proc_variant1 M.return)
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 M.return)
        ~eq:
          (F.proc_variant2 (fun (l, r) ->
               let open M.Let_syntax in
               let%bind l' = map_m l ~f in
               let%map r' = map_m r ~f in
               (l', r') ))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module A = Atomic_load.On_lvalues.On_monad (M)
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x
        ~bool_lit:(F.proc_variant1 M.return)
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 f)
        ~eq:
          (F.proc_variant2 (fun (l, r) ->
               let open M.Let_syntax in
               let%bind l' = map_m l ~f in
               let%map r' = map_m r ~f in
               (l', r') ))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_identifiers :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Identifier.t =
  Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)

module Type_check (E : Env.S) = struct
  module Lv = Lvalue.Type_check (E)
  module Ld = Atomic_load.Type_check (E)

  let type_of_constant : Constant.t -> Type.t Or_error.t = function
    | Char _ ->
        Or_error.unimplemented "char type"
    | Float _ ->
        Or_error.unimplemented "float type"
    | Integer _ ->
        Or_error.return Type.(normal Basic.int)

  let rec type_of : t -> Type.t Or_error.t = function
    | Bool_lit _ ->
        Or_error.return Type.(normal Basic.bool)
    | Constant k ->
        type_of_constant k
    | Lvalue l ->
        Lv.type_of l
    | Eq (l, r) ->
        type_of_relational l r
    | Atomic_load ld ->
        Ld.type_of ld

  and type_of_relational (l : t) (r : t) : Type.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map _ = type_of l and _ = type_of r in
    Type.(normal Basic.bool)
end

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Quickcheck.Observer.(
    fixed_point (fun mu ->
        unmap ~f:anonymise
          [%quickcheck.observer:
            [ `A of Bool.t
            | `B of Constant.t
            | `C of Lvalue.t
            | `D of [%custom mu] * [%custom mu]
            | `E of Atomic_load.t ]] ))

module Quickcheck_int_values (E : Env.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  module Gen = Base_quickcheck.Generator
  module Obs = Base_quickcheck.Observer
  module Snk = Base_quickcheck.Shrinker

  (** Generates the terminal integer expressions. *)
  let base_generators : t Gen.t list =
    (* Use thunks and let-modules here to prevent accidentally evaluating a
       generator that can't possibly work---eg, an atomic load when we don't
       have any atomic variables. *)
    List.map ~f:Core_kernel.Quickcheck.Generator.of_fun
      (List.filter_opt
         [ Some (fun () -> Gen.map ~f:constant Constant.gen_int32_constant)
         ; Option.some_if (E.has_atomic_int_variables ()) (fun () ->
               let module A = Atomic_load.Quickcheck_atomic_ints (E) in
               Gen.map ~f:atomic_load [%quickcheck.generator: A.t] )
         ; Option.some_if (E.has_int_variables ()) (fun () ->
               let module L = Lvalue.Quickcheck_int_values (E) in
               Gen.map ~f:lvalue [%quickcheck.generator: L.t] ) ])

  (* let recursive_generators (_mu : t Gen.t) : t Gen.t list = [] (* No
     useful recursive expression types yet. *) ;; *)

  let quickcheck_generator : t Gen.t = Gen.union base_generators

  (* ~f:recursive_generators *)

  let quickcheck_observer : t Obs.t = quickcheck_observer

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Snk.t = Snk.atomic
end

let test_int_values_liveness_on_mod (module E : Env.S) : unit =
  let module Ty = Type_check (E) in
  let module Q = Quickcheck_int_values (E) in
  Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t] ~f:(fun e ->
      Type.(
        [%compare.equal: t Or_error.t] (Ty.type_of e)
          (Or_error.return (normal Basic.int))) )

let test_int_values_distinctiveness_on_mod (module E : Env.S) : unit =
  let module Ty = Type_check (E) in
  let module Q = Quickcheck_int_values (E) in
  Quickcheck.test_distinct_values ~trials:20 ~distinct_values:5
    [%quickcheck.generator: Q.t] ~sexp_of:[%sexp_of: t]
    ~compare:[%compare: t]

let%test_unit "Quickcheck_int_values: liveness" =
  test_int_values_liveness_on_mod (Lazy.force Env.test_env_mod)

let%test_unit "Quickcheck_int_values: liveness (environment has only \
               atomic_int*)" =
  test_int_values_liveness_on_mod
    (Lazy.force Env.test_env_atomic_ptrs_only_mod)

let%test_unit "Quickcheck_int_values: liveness (environment is empty)" =
  test_int_values_liveness_on_mod (Lazy.force Env.empty_env_mod)

let%test_unit "Quickcheck_int_values: distinctiveness" =
  test_int_values_distinctiveness_on_mod (Lazy.force Env.test_env_mod)

let%test_unit "Quickcheck_int_values: distinctiveness (environment has \
               only atomic_int*)" =
  test_int_values_distinctiveness_on_mod
    (Lazy.force Env.test_env_atomic_ptrs_only_mod)

let%test_unit "Quickcheck_int_values: distinctiveness (environment is empty)"
    =
  test_int_values_distinctiveness_on_mod (Lazy.force Env.empty_env_mod)

module Quickcheck_bool_values (E : Env.S) : sig
  type nonrec t = t [@@deriving sexp_of]

  include Quickcheck.S with type t := t
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  module Gen = Quickcheck.Generator
  module Obs = Quickcheck.Observer
  module Snk = Quickcheck.Shrinker
  module Iv = Quickcheck_int_values (E)

  let gen_int_relational : t Gen.t =
    let open Gen.Let_syntax in
    let%bind l = [%quickcheck.generator: Iv.t] in
    let%map r = [%quickcheck.generator: Iv.t] in
    (* Only relational operation available atm. *)
    Eq (l, r)

  let gen_const : t Gen.t =
    Gen.map ~f:bool_lit [%quickcheck.generator: bool]

  let quickcheck_generator : t Gen.t =
    Gen.union [gen_int_relational; gen_const]

  let quickcheck_observer : t Obs.t = quickcheck_observer

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Snk.t = Snk.empty ()
end

let test_all_expressions_have_type
    (f : (module Env.S) -> (module Base_quickcheck.Test.S with type t = t))
    (ty : Type.t) : unit =
  let env = Lazy.force Env.test_env_mod in
  let (module Q) = f env in
  let module Ty = Type_check ((val env)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:(fun e ->
      [%test_result: Type.t Or_error.t] (Ty.type_of e) ~here:[[%here]]
        ~equal:[%compare.equal: Type.t Or_error.t]
        ~expect:(Or_error.return ty) )

let%test_module "tests using the standard environment" =
  ( module struct
    let test_all_expressions_in_env
        (f :
          (module Env.S) -> (module Base_quickcheck.Test.S with type t = t))
        : unit =
      let (module E) = Lazy.force Env.test_env_mod in
      let (module Q) = f (module E) in
      Base_quickcheck.Test.run_exn
        (module Q)
        ~f:
          ([%test_pred: t]
             (On_identifiers.for_all ~f:(Ac.C_id.Map.mem E.env))
             ~here:[[%here]])

    let%test_unit "Quickcheck_int_values: all expressions have 'int' type" =
      test_all_expressions_have_type
        (fun e -> (module Quickcheck_int_values ((val e))))
        Type.(normal Basic.int)

    let%test_unit "Quickcheck_int_values: all referenced variables in \
                   environment" =
      test_all_expressions_in_env (fun e ->
          (module Quickcheck_int_values ((val e))) )

    let%test_unit "Quickcheck_bool_values: all expressions have 'bool' type"
        =
      test_all_expressions_have_type
        (fun e -> (module Quickcheck_bool_values ((val e))))
        Type.(normal Basic.bool)

    let%test_unit "Quickcheck_bool_values: all referenced variables in \
                   environment" =
      test_all_expressions_in_env (fun e ->
          (module Quickcheck_bool_values ((val e))) )
  end )
