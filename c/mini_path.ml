(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

include Mini_path_intf

module Make_statement_list (M : S_statement)
  : S_statement_list with type target = M.target = struct
  type target = M.target

  let insert_stm (path : stm_hole list_path) (stm : Mini.Statement.t) (dest : target list)
    : target list Or_error.t =
    match path with
    | Insert_at index ->
      Alter_list.insert dest index (M.lift_stm stm)
    | At { index; rest } ->
      Alter_list.replace dest index
        ~f:(fun x -> Or_error.(M.insert_stm rest stm x >>| Option.some))
  ;;

  let gen_insert_stm_on (index : int) (single_dest : target)
    : stm_hole list_path Quickcheck.Generator.t list =
    let insert_after =
      Quickcheck.Generator.return (Insert_at (index + 1))
    in
    let insert_into =
      single_dest
      |> M.try_gen_insert_stm
      |> Option.map
        ~f:(Quickcheck.Generator.map
              ~f:(fun rest -> At { index; rest })
           )
      |> Option.to_list
    in
    insert_after :: insert_into
  ;;

  let gen_insert_stm (dest : target list)
    : stm_hole list_path Quickcheck.Generator.t =
    Quickcheck.Generator.union
      (Quickcheck.Generator.return (Insert_at 0)
       :: List.concat_mapi ~f:gen_insert_stm_on dest)
  ;;
end

module rec Statement
  : S_statement with type target = Mini.Statement.t = struct
  type target = Mini.Statement.t

  let lift_stm = Fn.id
  let lower_stm = Fn.id

  let in_if_error (dest : Mini.Statement.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message "Invalid target for 'in_if' path" [%here]
          ~target:(dest : Mini.Statement.t)]
  ;;

  let insert_stm_in_if (rest : stm_hole if_path)
      (stm : Mini.Statement.t)
      (dest : Mini.Statement.t)
    : Mini.Statement.t Or_error.t =

    Mini.Statement.map dest
      ~if_stm:(fun ifs -> Or_error.(ifs |> If_statement.insert_stm rest stm >>| Mini.Statement.if_stm))
      ~assign:(in_if_error dest)
      ~atomic_cmpxchg:(in_if_error dest)
      ~atomic_store:(in_if_error dest)
      ~nop:(in_if_error dest)
  ;;

  let insert_stm (path : stm_hole stm_path)
      (stm : Mini.Statement.t) (dest : Mini.Statement.t)
    : Mini.Statement.t Or_error.t =
    match path with
    | In_if rest ->
      insert_stm_in_if rest stm dest
    | This ->
      Or_error.error_s [%message "Can't insert a statement onto a statement" [%here]]
  ;;

  let gen_if_stm_insert_stm
      (i : Mini.If_statement.t)
    : stm_hole stm_path Quickcheck.Generator.t =
    Quickcheck.Generator.map (If_statement.gen_insert_stm i)
      ~f:(fun x -> In_if x)
  ;;

  let try_gen_insert_stm
    : Mini.Statement.t -> (stm_hole stm_path Quickcheck.Generator.t option) =
    Mini.Statement.map
      ~if_stm:(Fn.compose Option.some gen_if_stm_insert_stm)
      ~assign:(Fn.const None)
      ~atomic_cmpxchg:(Fn.const None)
      ~atomic_store:(Fn.const None)
      ~nop:(Fn.const None)
  ;;
end
and Statement_list : S_statement_list with type target = Mini.Statement.t =
  Make_statement_list (Statement)
and If_statement
  : S_if_statement with type target = Mini.If_statement.t = struct
  type target = Mini.If_statement.t

  module B = Mini.If_statement.Base_map (Or_error)

  let insert_stm
      (path : stm_hole if_path)
      (stm  : Mini.Statement.t)
      (ifs  : Mini.If_statement.t)
    : Mini.If_statement.t Or_error.t =
    match path with
    | Block { branch; rest } ->
      let t_branch, f_branch =
        ( (if branch
           then Statement_list.insert_stm rest stm
           else Or_error.return)
        , (if branch
           then Or_error.return
           else Statement_list.insert_stm rest stm
          )
        )
      in
      B.bmap ifs ~cond:(Or_error.return) ~t_branch ~f_branch
    | Cond ->
      Or_error.error_string "Not a statement insertion path"
  ;;

  let gen_insert_stm_for_branch
      (branch : bool)
      (branch_stms : Mini.Statement.t list)
      : stm_hole if_path Quickcheck.Generator.t =
    Quickcheck.Generator.map
      ~f:(fun rest -> Block { branch; rest })
      (Statement_list.gen_insert_stm branch_stms)

  let gen_insert_stm
      (ifs : Mini.If_statement.t)
    : stm_hole if_path Quickcheck.Generator.t =
    Quickcheck.Generator.union
      [ gen_insert_stm_for_branch true  (Mini.If_statement.t_branch ifs)
      ; gen_insert_stm_for_branch false (Mini.If_statement.f_branch ifs)
      ]
  ;;
end

module Function
  : S_function with type target := Mini.Function.t = struct
  type target = Mini.Function.t

  let gen_insert_stm (func : target)
    : stm_hole function_path Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (Statement_list.gen_insert_stm (Mini.Function.body_stms func))
      ~f:(fun path -> On_statements path)
  ;;

  let insert_stm
      (path : stm_hole function_path)
      (stm : Mini.Statement.t) (func : target) : target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | On_statements rest ->
      let%map body_stms' =
        Statement_list.insert_stm rest stm (Mini.Function.body_stms func)
      in Mini.Function.with_body_stms func body_stms'
  ;;
end

module Program : S_program with type target := Mini.Program.t = struct
  type target = Mini.Program.t

  let gen_insert_stm (prog : target)
    : stm_hole program_path Quickcheck.Generator.t =
    let prog_gens =
      List.mapi (Mini.Program.functions prog)
        ~f:(fun index (_, func) ->
            Quickcheck.Generator.map
              (Function.gen_insert_stm func)
              ~f:(fun rest -> On_program { index; rest })
          )
    in Quickcheck.Generator.union prog_gens
  ;;

  let insert_stm
      (path : stm_hole program_path)
      (stm : Mini.Statement.t)
      (prog : target) : target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | On_program { index; rest } ->
      let functions = Mini.Program.functions prog in
      let%map functions' = Alter_list.replace functions index
          ~f:(fun (name, func) ->
              let%map func' = Function.insert_stm rest stm func in
              Some (name, func'))
      in Mini.Program.with_functions prog functions'
  ;;
end
