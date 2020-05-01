(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** This interface intentionally left blank. *)

open Base

open struct
  module Ac = Act_common
  module Src = Act_c_mini
  module Ast = Act_c_lang.Ast
end

let%test_module "decls" =
  ( module struct
    let%test_module "examples" =
      ( module struct
        let test (x : Src.Initialiser.t Ac.C_named.t) =
          let rx = Src.Reify_prim.decl x in
          Fmt.pr "@[%a@]@." Act_c_lang.Ast.Decl.pp rx

        let%expect_test "atomic bool without known value" =
          test
            (Ac.C_named.make ~name:(Ac.C_id.of_string "bar")
               (Src.Initialiser.make
                  ~ty:(Src.Type.bool ~is_atomic:true ())
                  ())) ;
          [%expect {| atomic_bool bar; |}]

        let%expect_test "volatile int with known value" =
          test
            (Ac.C_named.make ~name:(Ac.C_id.of_string "foo")
               (Src.Initialiser.make
                  ~ty:(Src.Type.int ~is_volatile:true ())
                  ~value:(Src.Constant.int 42) ())) ;
          [%expect {| int volatile foo = 42; |}]
      end )

    let%test_unit "round trip" =
      Base_quickcheck.Test.run_exn
        ( module struct
          type t = Src.Initialiser.t Ac.C_named.t
          [@@deriving sexp, quickcheck]
        end )
        ~f:(fun init ->
          [%test_result: Src.Initialiser.t Ac.C_named.t Or_error.t]
            ~here:[[%here]] ~expect:(Ok init)
            (Src.Convert_prim.decl (Src.Reify_prim.decl init)))
  end )
