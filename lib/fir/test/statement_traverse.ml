(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fir
  module Stm = Statement
end

let%test_module "On_lvalues and On_addresses" =
  ( module struct
    module M = Src.Statement_traverse.With_meta (Unit)

    let stm =
      Src.(
        Statement.(
          Stm.mkwhile
            ~cond:(Expression.of_variable_str_exn "so")
            [ Stm.mkif
                ~cond:
                  (Expression.lvalue
                     Lvalue.(deref (of_variable_str_exn "i")))
                [ prim ()
                    (Prim_statement.atomic_store
                       (Src.Atomic_store.make ~mo:Relaxed
                          ~src:(Expression.of_variable_str_exn "herd")
                          ~dst:Address.(ref (of_variable_str_exn "u")))) ]
                [ prim ()
                    (Prim_statement.goto
                       (Act_common.C_id.of_string "not_you")) ]
            ; Stm.mkif
                ~cond:
                  Expression.(
                    Infix.(
                      of_variable_str_exn "liek"
                      && of_variable_str_exn "mudkipz"))
                [ prim ()
                    (Prim_statement.label
                       (Act_common.C_id.of_string "not_you")) ]
                [] ]))

    let print_exprs : Act_litmus_c.Ast.Expr.t list -> unit =
      Fmt.(pr "@[<v>%a@]@." (list ~sep:sp Act_litmus_c.Ast.Expr.pp))

    let%expect_test "coalesce addresses to list" =
      stm |> M.On_addresses.to_list
      |> List.map ~f:Act_litmus_c.Reify_prim.address
      |> print_exprs ;
      [%expect
        {|
        so
        *i
        herd
        &u
        liek
        mudkipz |}]
  end )

let%test_module "On_expressions" =
  ( module struct
    module M = Src.Statement_traverse.With_meta (Unit)

    let%expect_test "replace all expressions" =
      let stm =
        Src.(
          Statement.(
            Stm.mkwhile
              [ Stm.mkif ~cond:Src.Expression.falsehood
                  [ prim ()
                      (Prim_statement.assign
                         (Assign.make
                            ~lvalue:(Lvalue.of_variable_str_exn "r0")
                            ~rvalue:(Expression.int_lit 1))) ]
                  []
              ; Stm.mkif
                  [ prim ()
                      (Prim_statement.atomic_store
                         (Atomic_store.make ~mo:Seq_cst
                            ~src:(Expression.int_lit 27)
                            ~dst:(Address.of_variable_str_exn "x"))) ]
                  [ prim ()
                      (Prim_statement.atomic_store
                         (Atomic_store.make ~mo:Seq_cst
                            ~src:(Expression.int_lit 53)
                            ~dst:(Address.of_variable_str_exn "x"))) ] ]))
      in
      let stm' =
        M.On_expressions.map stm ~f:(fun _ -> Src.Expression.int_lit 2)
      in
      Fmt.pr "@[%a@]@." Act_litmus_c.Reify_stm.pp stm' ;
      [%expect
        {|
        while (2)
        {
            if (2) { r0 = 2; }
            if (2) { atomic_store_explicit(x, 2, memory_order_seq_cst); } else
            { atomic_store_explicit(x, 2, memory_order_seq_cst); }
        } |}]
  end )

let%test_module "On_primitives" =
  ( module struct
    module M = Src.Statement_traverse.With_meta (Unit)

    let%expect_test "replace all primitives" =
      let stm =
        Src.(
          Statement.(
            Stm.mkwhile
              [ Stm.mkif
                  [ prim ()
                      (Prim_statement.label
                         (Act_common.C_id.of_string "kappa")) ]
                  []
              ; Stm.mkif
                  [ prim ()
                      (Prim_statement.label
                         (Act_common.C_id.of_string "keepo")) ]
                  [ prim ()
                      (Prim_statement.atomic_fence
                         (Src.Atomic_fence.make ~mode:Thread ~mo:Seq_cst)) ]
              ; prim ()
                  (Prim_statement.goto (Act_common.C_id.of_string "kappa"))
              ]))
      in
      let stm' =
        M.On_primitives.map stm ~f:(fun _ -> Src.Prim_statement.return)
      in
      Fmt.pr "@[%a@]@." Act_litmus_c.Reify_stm.pp stm' ;
      [%expect
        {|
        while (true)
        { if (true) { return; } if (true) { return; } else { return; } return; } |}]
  end )
