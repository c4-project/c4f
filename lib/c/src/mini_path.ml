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
module Tx = Travesty_base_exts
include Mini_path_intf

module Make_statement_list (M : S_statement) :
  S_statement_list with type target = M.target = struct
  type target = M.target

  let insert_stm (path : stm_hole list_path) (stm : Mini.Statement.t)
      (dest : target list) : target list Or_error.t =
    match path with
    | Insert_at index ->
        Tx.List.insert dest index (M.lift_stm stm)
    | At {index; rest} ->
        Tx.List.With_errors.replace_m dest index ~f:(fun x ->
            Or_error.(M.insert_stm rest stm x >>| Option.some))

  let transform_stm (path : on_stm list_path)
      ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t)
      (dest : target list) : target list Or_error.t =
    match path with
    | At {index; rest} ->
        Tx.List.With_errors.replace_m dest index ~f:(fun x ->
            Or_error.(M.transform_stm rest ~f x >>| Option.some))
    | Insert_at _ ->
        Or_error.error_s
          [%message
            "Can't use insert-at path to transform statements" ~here:[%here]
              ~path:(path : on_stm list_path)]

  let gen_insert_stm_on (index : int) (single_dest : target) :
      stm_hole list_path Quickcheck.Generator.t list =
    let insert_after =
      Quickcheck.Generator.return (Insert_at (index + 1))
    in
    let insert_into =
      single_dest |> M.try_gen_insert_stm
      |> Option.map
           ~f:(Quickcheck.Generator.map ~f:(fun rest -> At {index; rest}))
      |> Option.to_list
    in
    insert_after :: insert_into

  let gen_insert_stm (dest : target list) :
      stm_hole list_path Quickcheck.Generator.t =
    Quickcheck.Generator.union
      ( Quickcheck.Generator.return (Insert_at 0)
      :: List.concat_mapi ~f:gen_insert_stm_on dest )
end

module rec Statement : (S_statement with type target = Mini.Statement.t) =
struct
  type target = Mini.Statement.t

  let lift_stm = Fn.id

  let lower_stm = Fn.id

  let in_if_error (dest : Mini.Statement.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here]
          ~target:(dest : Mini.Statement.t)]

  let handle_in_if (dest : Mini.Statement.t)
      ~(f : Mini.If_statement.t -> Mini.If_statement.t Or_error.t) :
      Mini.Statement.t Or_error.t =
    Mini.Statement.map dest
      ~if_stm:(fun ifs -> Or_error.(ifs |> f >>| Mini.Statement.if_stm))
      ~assign:(in_if_error dest) ~atomic_cmpxchg:(in_if_error dest)
      ~atomic_store:(in_if_error dest) ~nop:(in_if_error dest)

  let insert_stm (path : stm_hole stm_path) (stm : Mini.Statement.t)
      (dest : Mini.Statement.t) : Mini.Statement.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.insert_stm rest stm) dest
    | This ->
        Or_error.error_s
          [%message "Can't insert a statement onto a statement" [%here]]

  let transform_stm (path : on_stm stm_path)
      ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t)
      (dest : Mini.Statement.t) : Mini.Statement.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.transform_stm rest ~f) dest
    | This ->
        f dest

  let gen_if_stm_insert_stm (i : Mini.If_statement.t) :
      stm_hole stm_path Quickcheck.Generator.t =
    Quickcheck.Generator.map (If_statement.gen_insert_stm i) ~f:(fun x ->
        In_if x)

  let try_gen_insert_stm :
      Mini.Statement.t -> stm_hole stm_path Quickcheck.Generator.t option =
    Mini.Statement.map
      ~if_stm:(Fn.compose Option.some gen_if_stm_insert_stm)
      ~assign:(Fn.const None) ~atomic_cmpxchg:(Fn.const None)
      ~atomic_store:(Fn.const None) ~nop:(Fn.const None)
end

and Statement_list :
  (S_statement_list with type target = Mini.Statement.t) =
  Make_statement_list (Statement)

and If_statement : (S_if_statement with type target = Mini.If_statement.t) =
struct
  type target = Mini.If_statement.t

  module B = Mini.If_statement.Base_map (Or_error)

  let handle_stm (type a) (path : a if_path)
      ~(f :
            a list_path
         -> Mini.Statement.t list
         -> Mini.Statement.t list Or_error.t) (ifs : Mini.If_statement.t) :
      Mini.If_statement.t Or_error.t =
    match path with
    | Block {branch; rest} ->
        let t_branch, f_branch =
          ( (if branch then f rest else Or_error.return)
          , if branch then Or_error.return else f rest )
        in
        B.bmap ifs ~cond:Or_error.return ~t_branch ~f_branch
    | Cond ->
        Or_error.error_string "Not a statement path"

  let insert_stm (path : stm_hole if_path) (stm : Mini.Statement.t) :
      Mini.If_statement.t -> Mini.If_statement.t Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : on_stm if_path)
      ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t) :
      Mini.If_statement.t -> Mini.If_statement.t Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)

  let gen_insert_stm_for_branch (branch : bool)
      (branch_stms : Mini.Statement.t list) :
      stm_hole if_path Quickcheck.Generator.t =
    Quickcheck.Generator.map
      ~f:(fun rest -> Block {branch; rest})
      (Statement_list.gen_insert_stm branch_stms)

  let gen_insert_stm (ifs : Mini.If_statement.t) :
      stm_hole if_path Quickcheck.Generator.t =
    Quickcheck.Generator.union
      [ gen_insert_stm_for_branch true (Mini.If_statement.t_branch ifs)
      ; gen_insert_stm_for_branch false (Mini.If_statement.f_branch ifs) ]
end

let%test_unit "insertions into an empty list are always at index 0" =
  Quickcheck.test (Statement_list.gen_insert_stm [])
    ~sexp_of:[%sexp_of: stm_hole list_path] ~f:(function
    | Insert_at 0 ->
        ()
    | _ ->
        failwith "Unexpected path")

module Function : S_function with type target := Mini.Function.t = struct
  type target = Mini.Function.t

  let gen_insert_stm (func : target) :
      stm_hole function_path Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (Statement_list.gen_insert_stm (Mini.Function.body_stms func))
      ~f:(fun path -> On_statements path)

  let handle_stm (type a) (path : a function_path)
      ~(f :
            a list_path
         -> Mini.Statement.t list
         -> Mini.Statement.t list Or_error.t) (func : Mini.Function.t) :
      Mini.Function.t Or_error.t =
    match path with
    | On_statements rest ->
        Or_error.(
          f rest (Mini.Function.body_stms func)
          >>| Mini.Function.with_body_stms func)

  let insert_stm (path : stm_hole function_path) (stm : Mini.Statement.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : on_stm function_path)
      ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)
end

module Program : S_program with type target := Mini.Program.t = struct
  type target = Mini.Program.t

  let gen_insert_stm (prog : target) :
      stm_hole program_path Quickcheck.Generator.t =
    let prog_gens =
      List.mapi (Mini.Program.functions prog) ~f:(fun index (_, func) ->
          Quickcheck.Generator.map (Function.gen_insert_stm func)
            ~f:(fun rest -> On_program {index; rest}))
    in
    Quickcheck.Generator.union prog_gens

  let handle_stm (type a) (path : a program_path)
      ~(f :
         a function_path -> Mini.Function.t -> Mini.Function.t Or_error.t)
      (prog : target) : target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | On_program {index; rest} ->
        let functions = Mini.Program.functions prog in
        let%map functions' =
          Tx.List.With_errors.replace_m functions index
            ~f:(fun (name, func) ->
              let%map func' = f rest func in
              Some (name, func'))
        in
        Mini.Program.with_functions prog functions'

  let insert_stm (path : stm_hole program_path) (stm : Mini.Statement.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Function.insert_stm rest stm)

  let transform_stm (path : on_stm program_path)
      ~(f : Mini.Statement.t -> Mini.Statement.t Or_error.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(Function.transform_stm ~f)
end
