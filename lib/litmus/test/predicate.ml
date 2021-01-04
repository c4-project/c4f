(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = Act_litmus

let pp_pred : int Src.Predicate.t Fmt.t = Src.Predicate.pp ~pp_const:Int.pp

let eval_int : int Src.Predicate.Element.t -> bool = function
  | Eq (_, y) ->
      Int.is_non_negative y
  | Bool x ->
      x

let eval_pred : int Src.Predicate.t -> bool =
  Src.Postcondition_eval.eval_pred ~elt:eval_int

let eval_fail_message ~(expect : int Src.Predicate.t)
    (actual : int Src.Predicate.t) : string =
  Fmt.str "'%a' does not evaluate to the same result as '%a'" pp_pred actual
    pp_pred expect

let test_eval ~(here : _ list) ~(expect : int Src.Predicate.t)
    (actual : int Src.Predicate.t) : unit =
  [%test_result: bool] ~here
    ~message:(eval_fail_message ~expect actual)
    ~expect:(eval_pred expect) (eval_pred actual)

let%test_module "optimising_and" =
  ( module struct
    let opt_and_list (xs : int Src.Predicate.t list) : int Src.Predicate.t =
      Src.Predicate.optimising_and_seq (Sequence.of_list xs)

    let%test_unit "optimising_and behaves like and" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t * int Src.Predicate.t
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun (l, r) ->
          test_eval ~here:[[%here]]
            ~expect:Src.Predicate.Infix.(l && r)
            Src.Predicate.Infix.(l &&+ r))

    let%test_unit "optimising_and_seq on singleton is the identity" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t [@@deriving sexp, quickcheck]
        end )
        ~f:(fun x ->
          [%test_result: int Src.Predicate.t] ~here:[[%here]] ~expect:x
            (Src.Predicate.optimising_and_seq (Sequence.singleton x)))

    let%test_unit "optimising_and_seq on list append behaves like and of \
                   optimising_and_seqs" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t list * int Src.Predicate.t list
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun (l, r) ->
          test_eval ~here:[[%here]]
            ~expect:Src.Predicate.(Infix.(opt_and_list l && opt_and_list r))
            (opt_and_list (l @ r)))
  end )

let%test_module "optimising_or" =
  ( module struct
    let opt_or_list (xs : int Src.Predicate.t list) : int Src.Predicate.t =
      Src.Predicate.optimising_or_seq (Sequence.of_list xs)

    let%test_unit "optimising_or behaves like or" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t * int Src.Predicate.t
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun (l, r) ->
          test_eval ~here:[[%here]]
            ~expect:Src.Predicate.Infix.(l || r)
            Src.Predicate.Infix.(l ||+ r))

    let%test_unit "optimising_or_seq on singleton is the identity" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t [@@deriving sexp, quickcheck]
        end )
        ~f:(fun x ->
          [%test_result: int Src.Predicate.t] ~here:[[%here]] ~expect:x
            (Src.Predicate.optimising_or_seq (Sequence.singleton x)))

    let%test_unit "optimising_or_seq on list append behaves like or of \
                   optimising_or_seqs" =
      Test.run_exn
        ( module struct
          type t = int Src.Predicate.t list * int Src.Predicate.t list
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun (l, r) ->
          test_eval ~here:[[%here]]
            ~expect:Src.Predicate.(Infix.(opt_or_list l || opt_or_list r))
            (opt_or_list (l @ r)))
  end )
