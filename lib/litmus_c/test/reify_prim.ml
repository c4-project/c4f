(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** This interface intentionally left blank. *)

open Base

open struct
  module Ac = C4f_common
  module Src = C4f_litmus_c
  module Fir = C4f_fir
  module Ast = C4f_litmus_c.Ast
end

let%test_module "decls" =
  ( module struct
    let%test_module "examples" =
      ( module struct
        let test (x : Fir.Initialiser.t Ac.C_named.t) =
          let rx = Src.Reify_prim.decl x in
          Fmt.pr "@[%a@]@." C4f_litmus_c.Ast.Decl.pp rx

        let%expect_test "atomic bool" =
          test
            (Ac.C_named.make ~name:(Ac.C_id.of_string "bar")
               Fir.
                 { Initialiser.ty= Type.bool ~is_atomic:true ()
                 ; value= Constant.truth }) ;
          [%expect {| atomic_bool bar = true; |}]

        let%expect_test "volatile int" =
          test
            (Ac.C_named.make ~name:(Ac.C_id.of_string "foo")
               Fir.
                 { Initialiser.ty= Type.int ~is_volatile:true ()
                 ; value= Constant.int 42 }) ;
          [%expect {| int volatile foo = 42; |}]
      end )

    let%test_unit "round trip" =
      Base_quickcheck.Test.run_exn
        ( module struct
          type t = Fir.Initialiser.t Ac.C_named.t
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun init ->
          [%test_result: Fir.Initialiser.t Ac.C_named.t Or_error.t]
            ~here:[[%here]] ~expect:(Ok init)
            (Src.Abstract_prim.decl (Src.Reify_prim.decl init)))
  end )
