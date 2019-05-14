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

type t = Variable of C_identifier.t | Deref of t
[@@deriving sexp, variants, eq]

let rec reduce (lv : t) ~(variable : C_identifier.t -> 'a)
    ~(deref : 'a -> 'a) : 'a =
  match lv with
  | Variable v ->
      variable v
  | Deref rest ->
      deref (reduce rest ~variable ~deref)

let is_deref : t -> bool = function Deref _ -> true | Variable _ -> false

module On_identifiers :
  Travesty.Traversable.S0 with type t = t and type Elt.t = C_identifier.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = C_identifier

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x ~variable:(F.proc_variant1 f)
        ~deref:(F.proc_variant1 (map_m ~f))
  end
end)

let variable_of : t -> C_identifier.t = reduce ~variable:Fn.id ~deref:Fn.id

let variable_in_env (lv : t) ~(env : _ C_identifier.Map.t) : bool =
  C_identifier.Map.mem env (variable_of lv)

let%expect_test "variable_in_env: positive variable result, test env" =
  let env = Lazy.force Mini_env.test_env in
  Sexp.output_hum stdout
    [%sexp
      (variable_in_env ~env (Variable (C_identifier.of_string "foo")) : bool)] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Mini_env.test_env in
  Sexp.output_hum stdout
    [%sexp
      ( variable_in_env ~env (Variable (C_identifier.of_string "kappa"))
        : bool )] ;
  [%expect {| false |}]

let%expect_test "variable_in_env: positive deref result, test env" =
  let env = Lazy.force Mini_env.test_env in
  Sexp.output_hum stdout
    [%sexp
      ( variable_in_env ~env
          (Deref (Variable (C_identifier.of_string "bar")))
        : bool )] ;
  [%expect {| true |}]

let%expect_test "variable_in_env: negative variable result, test env" =
  let env = Lazy.force Mini_env.test_env in
  Sexp.output_hum stdout
    [%sexp
      ( variable_in_env ~env
          (Deref (Variable (C_identifier.of_string "keepo")))
        : bool )] ;
  [%expect {| false |}]

module Type_check (E : Mini_env.S) = struct
  let rec type_of : t -> Mini_type.t Or_error.t = function
    | Variable v ->
        Result.of_option
          (C_identifier.Map.find E.env v)
          ~error:
            (Error.create_s
               [%message
                 "Variable not in environment"
                   ~variable:(v : C_identifier.t)
                   ~environment:(E.env : Mini_type.t C_identifier.Map.t)])
    | Deref l ->
        Or_error.(l |> type_of >>= Mini_type.deref)
end

let%expect_test "Type-checking a valid normal variable lvalue" =
  let module T = Type_check ((val Lazy.force Mini_env.test_env_mod)) in
  let result = T.type_of (Variable (C_identifier.of_string "foo")) in
  Sexp.output_hum stdout [%sexp (result : Mini_type.t Or_error.t)] ;
  [%expect {| (Ok (Normal int)) |}]

let%expect_test "Type-checking an invalid deferencing variable lvalue" =
  let module T = Type_check ((val Lazy.force Mini_env.test_env_mod)) in
  let result =
    T.type_of (Deref (Variable (C_identifier.of_string "foo")))
  in
  Sexp.output_hum stdout [%sexp (result : Mini_type.t Or_error.t)] ;
  [%expect {| (Error "not a pointer type") |}]

let anonymise = function Variable v -> `A v | Deref d -> `B d

let deanonymise = function `A v -> Variable v | `B d -> Deref d

module Quickcheck_generic (Id : Quickcheck.S with type t := C_identifier.t) : sig
  type nonrec t = t [@@deriving sexp_of]

  include Quickcheck.S with type t := t
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.(
      recursive_union
        [map [%quickcheck.generator: Id.t] ~f:variable]
        ~f:(fun mu -> [map mu ~f:deref]))

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.(
      fixed_point (fun mu ->
          unmap ~f:anonymise
            [%quickcheck.observer: [`A of Id.t | `B of [%custom mu]]] ))

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Id.t | `B of [%custom mu]]] ))
end

module Quickcheck_id = Quickcheck_generic (C_identifier)

include (Quickcheck_id : module type of Quickcheck_id with type t := t)

let%test_unit "gen: distinctiveness" =
  Quickcheck.test_distinct_values ~sexp_of:[%sexp_of: t] ~trials:20
    ~distinct_values:5 ~compare:[%compare: t] [%quickcheck.generator: t]

let on_value_of_typed_id ~(id : C_identifier.t) ~(ty : Mini_type.t) : t =
  if Mini_type.is_pointer ty then Deref (Variable id) else Variable id

let%test_unit "on_value_of_typed_id: always takes basic type" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Tc = Type_check (E) in
  Base_quickcheck.Test.run_exn
    (module E.Random_var)
    ~f:(fun id ->
      let ty = C_identifier.Map.find_exn E.env id in
      [%test_result: Mini_type.t Or_error.t] ~here:[[%here]]
        (Tc.type_of (on_value_of_typed_id ~id ~ty))
        ~expect:(Or_error.return Mini_type.(normal (basic_type ty))) )

module Quickcheck_on_env (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of]

  include Quickcheck.S with type t := t
end =
  Quickcheck_generic (E.Random_var)

let%test_unit "Quickcheck_on_env: liveness" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_on_env (E) in
  Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t]
    ~f:(variable_in_env ~env:E.env)

let%test_unit "Quickcheck_on_env: generated underlying variables in \
               environment" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_on_env (E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in_env ~env:E.env))

module Quickcheck_int_values (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of]

  include Quickcheck.S with type t := t
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (Quickcheck.Generator.of_list (Map.to_alist (E.int_variables ())))
      ~f:(fun (id, ty) -> on_value_of_typed_id ~id ~ty)

  module Q = Quickcheck_on_env (E)

  let quickcheck_observer = [%quickcheck.observer: Q.t]

  let quickcheck_shrinker = [%quickcheck.shrinker: Q.t]
end

let%test_unit "Quickcheck_int_values: liveness" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_int_values (E) in
  Quickcheck.test_can_generate [%quickcheck.generator: Q.t]
    ~sexp_of:[%sexp_of: t]
    ~f:(variable_in_env ~env:E.env)

let%test_unit "Quickcheck_int_values: generated underlying variables in \
               environment" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_int_values (E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:([%test_pred: t] ~here:[[%here]] (variable_in_env ~env:E.env))

let%test_unit "Quickcheck_int_values: generated lvalues have 'int' type" =
  let (module E) = Lazy.force Mini_env.test_env_mod in
  let module Q = Quickcheck_int_values (E) in
  let module Tc = Type_check (E) in
  Base_quickcheck.Test.run_exn
    (module Q)
    ~f:(fun lv ->
      [%test_result: Mini_type.t Or_error.t] ~here:[[%here]] (Tc.type_of lv)
        ~expect:(Or_error.return Mini_type.(normal Basic.int)) )
