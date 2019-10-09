(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = Act_c_lang

let%test_unit "Pretty-printing and parsing postconditions is idempotent" =
  Test.run_exn
    ( module struct
      type t = Act_c_lang.Ast_basic.Constant.t Act_litmus.Postcondition.t
      [@@deriving sexp, quickcheck]
    end )
    ~f:(fun pcond ->
      let pcond_str : string =
        Fmt.str "@[%a@]"
          (Act_litmus.Postcondition.pp
             ~pp_const:Act_c_lang.Ast.Litmus_lang.Constant.pp)
          pcond
      in
      [%test_result:
        Act_c_lang.Ast.Litmus_lang.Constant.t Act_litmus.Postcondition.t
        Or_error.t] ~here:[[%here]]
        ~equal:
          [%compare.equal:
            Act_c_lang.Ast.Litmus_lang.Constant.t Act_litmus.Postcondition.t
            Or_error.t] ~expect:(Or_error.return pcond)
        ~message:
          (Printf.sprintf
             "Pretty-printing/parsing round-trip through '%s' failed"
             pcond_str)
        (Src.Frontend.Litmus_post.load_from_string pcond_str))
