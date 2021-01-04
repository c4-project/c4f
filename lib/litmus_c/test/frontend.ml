(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = C4f_litmus_c

let%test_unit "Pretty-printing and parsing postconditions is idempotent" =
  Test.run_exn
    ( module struct
      type t = C4f_litmus_c.Ast_basic.Constant.t C4f_litmus.Postcondition.t
      [@@deriving sexp, quickcheck]
    end )
    ~f:(fun pcond ->
      let pcond_str : string =
        Fmt.str "@[%a@]"
          (C4f_litmus.Postcondition.pp
             ~pp_const:C4f_litmus_c.Ast_basic.Constant.pp)
          pcond
      in
      [%test_result:
        C4f_litmus_c.Ast.Litmus_lang.Constant.t C4f_litmus.Postcondition.t
        Or_error.t] ~here:[[%here]]
        ~equal:
          [%compare.equal:
            C4f_litmus_c.Ast.Litmus_lang.Constant.t
            C4f_litmus.Postcondition.t
            Or_error.t]
        ~expect:(Or_error.return pcond)
        ~message:
          (Printf.sprintf
             "Pretty-printing/parsing round-trip through '%s' failed"
             pcond_str)
        (Src.Frontend.Litmus_post.load_from_string pcond_str))
