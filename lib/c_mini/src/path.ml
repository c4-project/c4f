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

open Base
module Tx = Travesty_base_exts

type on_expr = [%import: Path.on_expr] [@@deriving sexp_of]

type on_stm = [%import: Path.on_stm] [@@deriving sexp_of]

type stm_hole = [%import: Path.stm_hole] [@@deriving sexp_of]

(* We can't import these three, as they're recursive. *)
type 'a stm_path =
  | In_if : 'a if_path -> 'a stm_path
  | This : on_stm stm_path
[@@deriving sexp_of]

and 'a list_path =
  | Insert_at : int -> 'a list_path
  | At : {index: int; rest: 'a stm_path} -> 'a list_path
[@@deriving sexp_of]

and 'a if_path =
  | Block : {branch: bool; rest: 'a list_path} -> 'a if_path
  | Cond : on_expr if_path
[@@deriving sexp_of]

type 'a function_path = [%import: Path.function_path]

type 'a program_path = [%import: Path.program_path]

module type S_path = [%import: (module Path.S_path)]

module type S_statement = [%import: (module Path.S_statement)]

module type S_stm_container = [%import: (module Path.S_stm_container)]

module type S_if_statement = [%import: (module Path.S_if_statement)]

module type S_statement_list = [%import: (module Path.S_statement_list)]

module type S_function = [%import: (module Path.S_function)]

module type S_program = [%import: (module Path.S_program)]

module Make_statement_list (M : S_statement) :
  S_statement_list with type target = M.target = struct
  type target = M.target

  let insert_stm (path : stm_hole list_path) (stm : Statement.t)
      (dest : target list) : target list Or_error.t =
    match path with
    | Insert_at index ->
        Tx.List.insert dest index (M.lift_stm stm)
    | At {index; rest} ->
        Tx.List.With_errors.replace_m dest index ~f:(fun x ->
            Or_error.(M.insert_stm rest stm x >>| Option.some))

  let transform_stm (path : on_stm list_path)
      ~(f : Statement.t -> Statement.t Or_error.t) (dest : target list) :
      target list Or_error.t =
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
      stm_hole list_path Base_quickcheck.Generator.t list =
    let insert_after =
      Base_quickcheck.Generator.return (Insert_at (index + 1))
    in
    let insert_into =
      single_dest |> M.try_gen_insert_stm
      |> Option.map
           ~f:
             (Base_quickcheck.Generator.map ~f:(fun rest ->
                  At {index; rest}))
      |> Option.to_list
    in
    insert_after :: insert_into

  let gen_insert_stm (dest : target list) :
      stm_hole list_path Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      ( Base_quickcheck.Generator.return (Insert_at 0)
      :: List.concat_mapi ~f:gen_insert_stm_on dest )
end

module Stm = Statement

module rec Statement : (S_statement with type target = Stm.t) = struct
  type target = Stm.t

  let lift_stm = Fn.id

  let lower_stm = Fn.id

  let in_if_error (dest : Stm.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here] ~target:(dest : Stm.t)]

  let handle_in_if (dest : Stm.t) ~(f : Stm.If.t -> Stm.If.t Or_error.t) :
      Stm.t Or_error.t =
    Stm.map dest
      ~if_stm:(fun ifs -> Or_error.(ifs |> f >>| Stm.if_stm))
      ~assign:(in_if_error dest) ~atomic_cmpxchg:(in_if_error dest)
      ~atomic_store:(in_if_error dest) ~nop:(in_if_error dest)

  let insert_stm (path : stm_hole stm_path) (stm : Stm.t) (dest : Stm.t) :
      Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.insert_stm rest stm) dest

  let transform_stm (path : on_stm stm_path)
      ~(f : Stm.t -> Stm.t Or_error.t) (dest : Stm.t) : Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.transform_stm rest ~f) dest
    | This ->
        f dest

  let gen_if_stm_insert_stm (i : Stm.If.t) :
      stm_hole stm_path Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map (If_statement.gen_insert_stm i)
      ~f:(fun x -> In_if x)

  let try_gen_insert_stm :
      Stm.t -> stm_hole stm_path Base_quickcheck.Generator.t option =
    Stm.map
      ~if_stm:(Fn.compose Option.some gen_if_stm_insert_stm)
      ~assign:(Fn.const None) ~atomic_cmpxchg:(Fn.const None)
      ~atomic_store:(Fn.const None) ~nop:(Fn.const None)
end

and Statement_list : (S_statement_list with type target = Stm.t) =
  Make_statement_list (Statement)

and If_statement : (S_if_statement with type target = Stm.If.t) = struct
  type target = Stm.If.t

  module B = Stm.If.Base_map (Or_error)

  let handle_stm (type a) (path : a if_path)
      ~(f : a list_path -> Stm.t list -> Stm.t list Or_error.t)
      (ifs : Stm.If.t) : Stm.If.t Or_error.t =
    match path with
    | Block {branch; rest} ->
        let t_branch, f_branch =
          ( (if branch then f rest else Or_error.return)
          , if branch then Or_error.return else f rest )
        in
        B.bmap ifs ~cond:Or_error.return ~t_branch ~f_branch
    | Cond ->
        Or_error.error_string "Not a statement path"

  let insert_stm (path : stm_hole if_path) (stm : Stm.t) :
      Stm.If.t -> Stm.If.t Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : on_stm if_path) ~(f : Stm.t -> Stm.t Or_error.t)
      : Stm.If.t -> Stm.If.t Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)

  let gen_insert_stm_for_branch (branch : bool) (branch_stms : Stm.t list) :
      stm_hole if_path Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      ~f:(fun rest -> Block {branch; rest})
      (Statement_list.gen_insert_stm branch_stms)

  let gen_insert_stm (ifs : Stm.If.t) :
      stm_hole if_path Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      [ gen_insert_stm_for_branch true (Stm.If.t_branch ifs)
      ; gen_insert_stm_for_branch false (Stm.If.f_branch ifs) ]
end

let%test_unit "insertions into an empty list are always at index 0" =
  Base_quickcheck.Test.run_exn
    ( module struct
      type t = stm_hole list_path

      let sexp_of_t = [%sexp_of: stm_hole list_path]

      let quickcheck_generator = Statement_list.gen_insert_stm []

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end )
    ~f:(function Insert_at 0 -> () | _ -> failwith "Unexpected path")

module Fun = Function

module Function : S_function with type target := Function.t = struct
  type target = Function.t

  let gen_insert_stm (func : target) :
      stm_hole function_path Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      (Statement_list.gen_insert_stm (Function.body_stms func))
      ~f:(fun path -> On_statements path)

  let handle_stm (type a) (path : a function_path)
      ~(f : a list_path -> Stm.t list -> Stm.t list Or_error.t)
      (func : Function.t) : Function.t Or_error.t =
    match path with
    | On_statements rest ->
        Or_error.(
          f rest (Function.body_stms func) >>| Function.with_body_stms func)

  let insert_stm (path : stm_hole function_path) (stm : Stm.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : on_stm function_path)
      ~(f : Stm.t -> Stm.t Or_error.t) : target -> target Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)
end

module Program : S_program with type target := Program.t = struct
  type target = Program.t

  let gen_insert_stm (prog : target) :
      stm_hole program_path Base_quickcheck.Generator.t =
    let prog_gens =
      List.mapi (Program.functions prog) ~f:(fun index (_, func) ->
          Base_quickcheck.Generator.map (Function.gen_insert_stm func)
            ~f:(fun rest -> On_program {index; rest}))
    in
    Base_quickcheck.Generator.union prog_gens

  let handle_stm (type a) (path : a program_path)
      ~(f : a function_path -> Fun.t -> Fun.t Or_error.t) (prog : target) :
      target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | On_program {index; rest} ->
        let functions = Program.functions prog in
        let%map functions' =
          Tx.List.With_errors.replace_m functions index
            ~f:(fun (name, func) ->
              let%map func' = f rest func in
              Some (name, func'))
        in
        Program.with_functions prog functions'

  let insert_stm (path : stm_hole program_path) (stm : Stm.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Function.insert_stm rest stm)

  let transform_stm (path : on_stm program_path)
      ~(f : Stm.t -> Stm.t Or_error.t) : target -> target Or_error.t =
    handle_stm path ~f:(Function.transform_stm ~f)
end
