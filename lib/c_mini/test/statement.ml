(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_c_mini

let cond : Src.Expression.t = Src.Expression.bool_lit true

let mkif ?(cond : Src.Expression.t = cond) (ts : unit Src.Statement.t list)
    (fs : unit Src.Statement.t list) : unit Src.Statement.t =
  Src.Statement.(
    if_stm
      (If.make ~cond
         ~t_branch:(Src.Block.of_statement_list ts)
         ~f_branch:(Src.Block.of_statement_list fs)))

let mkwhile ?(cond : Src.Expression.t = cond)
    (xs : unit Src.Statement.t list) : unit Src.Statement.t =
  Src.Statement.(
    while_loop
      (While.make ~cond ~kind:`While ~body:(Src.Block.of_statement_list xs)))

let nop : unit Src.Statement.t = Src.Statement.prim () Src.Prim_statement.nop

let%test_module "has_if_statements" =
  ( module struct
    let test (s : unit Src.Statement.t) : unit =
      Act_utils.Io.print_bool (Src.Statement.has_if_statements s)

    let%expect_test "nop" = test nop ; [%expect {| false |}]

    let%expect_test "naked if statement" =
      test (mkif [] []) ;
      [%expect {| true |}]

    let%expect_test "while loop without if statement" =
      test (mkwhile []) ;
      [%expect {| false |}]

    let%expect_test "while loop with an if statement" =
      test (mkwhile [nop; mkif [] []; nop]) ;
      [%expect {| true |}]
  end )

let%test_module "has_while_loops" =
  ( module struct
    let test (s : unit Src.Statement.t) : unit =
      Act_utils.Io.print_bool (Src.Statement.has_while_loops s)

    let%expect_test "nop" = test nop ; [%expect {| false |}]

    let%expect_test "naked while loop" =
      test (mkwhile []) ;
      [%expect {| true |}]

    let%expect_test "if statement without while loop" =
      test (mkif [] []) ;
      [%expect {| false |}]

    let%expect_test "if statement with while loop in true branch" =
      test (mkif [nop; mkwhile []; nop] []) ;
      [%expect {| true |}]

    let%expect_test "if statement with while loop in false branch" =
      test (mkif [] [nop; mkwhile []; nop]) ;
      [%expect {| true |}]
  end )

let%test_module "On_lvalues and On_addresses" =
  ( module struct
    module M = Src.Statement.With_meta (Unit)

    let stm =
      Src.(
        Statement.(
          mkwhile
            ~cond:(Expression.of_variable_str_exn "so")
            [ mkif
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
            ; mkif
                ~cond:
                  Expression.(
                    Infix.(
                      of_variable_str_exn "liek"
                      && of_variable_str_exn "mudkipz"))
                [ prim ()
                    (Prim_statement.label
                       (Act_common.C_id.of_string "not_you")) ]
                [] ]))

    let print_exprs : Act_c_lang.Ast.Expr.t list -> unit =
      Fmt.(pr "@[<v>%a@]@." (list ~sep:sp Act_c_lang.Ast.Expr.pp))

    let%expect_test "coalesce lvalues to list" =
      stm |> M.On_lvalues.to_list
      |> List.map ~f:Src.Reify_expr.lvalue
      |> print_exprs ;
      [%expect
        {|
        so
        *i
        herd
        u
        liek
        mudkipz |}]

    let%expect_test "coalesce addresses to list" =
      stm |> M.On_addresses.to_list
      |> List.map ~f:Src.Reify_expr.address
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

let%test_module "On_primitives" =
  ( module struct
    module M = Src.Statement.With_meta (Unit)

    let%expect_test "replace all primitives" =
      let stm =
        Src.(
          Statement.(
            mkwhile
              [ mkif
                  [ prim ()
                      (Prim_statement.label
                         (Act_common.C_id.of_string "kappa")) ]
                  []
              ; mkif
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
      let rp = Src.Reify_stm.reify stm' in
      Fmt.pr "@[%a@]@." Act_c_lang.Ast.Stm.pp rp ;
      [%expect
        {|
        while (true)
        { if (true) { return; } if (true) { return; } else { return; } return; } |}]
  end )
