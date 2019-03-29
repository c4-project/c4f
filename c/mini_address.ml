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
open Utils

type t = Lvalue of Mini_lvalue.t | Ref of t
[@@deriving sexp, variants, eq]

let of_variable (v : C_identifier.t) : t = Lvalue (Mini_lvalue.variable v)

let of_variable_ref (v : C_identifier.t) : t = Ref (of_variable v)

let rec reduce (addr : t) ~(lvalue : Mini_lvalue.t -> 'a) ~(ref : 'a -> 'a)
    : 'a =
  match addr with
  | Lvalue lv ->
      lvalue lv
  | Ref rest ->
      ref (reduce rest ~lvalue ~ref)

module On_lvalues :
  Travesty.Traversable.S0_container
  with type t := t
   and type Elt.t = Mini_lvalue.t =
Travesty.Traversable.Make_container0 (struct
  type nonrec t = t

  module Elt = Mini_lvalue

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x ~lvalue:(F.proc_variant1 f)
        ~ref:(F.proc_variant1 (map_m ~f))
  end
end)

module Type_check (E : Mini_env.S) = struct
  module L = Mini_lvalue.Type_check (E)

  let type_of : t -> Mini_type.t Or_error.t =
    reduce ~lvalue:L.type_of ~ref:(Or_error.bind ~f:Mini_type.ref)
end

let%expect_test "Type-checking a valid normal variable lvalue" =
  let module T = Type_check ((val Lazy.force Mini_env.test_env_mod)) in
  let result = T.type_of (of_variable (C_identifier.of_string "foo")) in
  Sexp.output_hum stdout [%sexp (result : Mini_type.t Or_error.t)] ;
  [%expect {| (Ok (Normal int)) |}]

let%expect_test "Type-checking an valid reference lvalue" =
  let module T = Type_check ((val Lazy.force Mini_env.test_env_mod)) in
  let result = T.type_of (of_variable_ref (C_identifier.of_string "foo")) in
  Sexp.output_hum stdout [%sexp (result : Mini_type.t Or_error.t)] ;
  [%expect {| (Ok (Pointer_to int)) |}]

let anonymise = function Lvalue v -> `A v | Ref d -> `B d

let deanonymise = function `A v -> Lvalue v | `B d -> Ref d

module Quickcheck_generic
    (Lv : Quickcheckable.S with type t := Mini_lvalue.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.(
      recursive_union
        [map [%quickcheck.generator: Lv.t] ~f:lvalue]
        ~f:(fun mu -> [map mu ~f:ref]))

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.(
      fixed_point (fun mu ->
          unmap ~f:anonymise
            [%quickcheck.observer: [`A of Lv.t | `B of [%custom mu]]] ))

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Lv.t | `B of [%custom mu]]] ))
end

module Quickcheck_main = Quickcheck_generic (Mini_lvalue)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let on_address_of_typed_id ~(id : C_identifier.t) ~(ty : Mini_type.t) : t =
  let lv = of_variable id in
  if Mini_type.is_pointer ty then lv else ref lv

let%test_unit "on_address_of_typed_id: always takes pointer type" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Tc = Type_check (E) in
  Base_quickcheck.Test.run_exn
    (module E.Random_var)
    ~f:(fun id ->
      let ty = C_identifier.Map.find_exn E.env id in
      [%test_result: Mini_type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_address_of_typed_id ~id ~ty))
        ~expect:(Or_error.return Mini_type.(pointer_to (basic_type ty))) )

let lvalue_of : t -> Mini_lvalue.t = reduce ~lvalue:Fn.id ~ref:Fn.id

let variable_of (addr : t) : C_identifier.t =
  Mini_lvalue.variable_of (lvalue_of addr)

let%test_unit "variable_of: preserved by ref" =
  Base_quickcheck.Test.run_exn
    (module Quickcheck_main)
    ~f:(fun x ->
      [%test_eq: C_identifier.t] ~here:[[%here]] (variable_of x)
        (variable_of (ref x)) )

let%expect_test "variable_of: nested example" =
  let example =
    Ref
      (Ref
         (Lvalue
            (Mini_lvalue.deref
               (Mini_lvalue.variable (C_identifier.of_string "yorick")))))
  in
  let var = variable_of example in
  Fmt.pr "%a@." C_identifier.pp var ;
  [%expect {| yorick |}]

let variable_in_env (addr : t) ~(env : _ C_identifier.Map.t) : bool =
  Mini_lvalue.variable_in_env (lvalue_of addr) ~env

module Quickcheck_on_env (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end =
  Quickcheck_generic (Mini_lvalue.Quickcheck_on_env (E))

let variable_in (module E : Mini_env.S) (l : t) : bool =
  C_identifier.Map.mem E.env (variable_of l)

let%test_unit "Quickcheck_on_env: liveness" =
  let e = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_on_env ((val e)) in
  Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t] ~f:(variable_in e)

let%test_unit "Quickcheck_on_env: generated underlying variables in \
               environment" =
  let e = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_on_env ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in e))

module Quickcheck_atomic_int_pointers (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (Quickcheck.Generator.of_list
         (Map.to_alist (E.atomic_int_variables ())))
      ~f:(fun (id, ty) -> on_address_of_typed_id ~id ~ty)

  module Q = Quickcheck_on_env (E)

  let quickcheck_observer = [%quickcheck.observer: Q.t]

  let quickcheck_shrinker = [%quickcheck.shrinker: Q.t]
end

let%test_unit "Quickcheck_atomic_int_pointers: liveness" =
  let e = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_atomic_int_pointers ((val e)) in
  Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t] ~trials:20 ~f:(variable_in e)

let%test_unit "Quickcheck_atomic_int_pointers: generated underlying \
               variables in environment" =
  let e = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_atomic_int_pointers ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in e))

let%test_unit "Quickcheck_int_values: generated lvalues have '*atomic_int' \
               type" =
  let e = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_atomic_int_pointers ((val e)) in
  let module Tc = Type_check ((val e)) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:(fun lv ->
      [%test_result: Mini_type.t Or_error.t] ~here:[[%here]] (Tc.type_of lv)
        ~expect:(Or_error.return Mini_type.(pointer_to Basic.atomic_int)) )
