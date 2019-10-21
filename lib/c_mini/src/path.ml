(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts

module Make_statement_list (M : Path_types.S_statement) :
  Path_types.S_statement_list with type 'meta target = 'meta M.target =
struct
  type 'meta target = 'meta M.target

  let insert_stm (path : Path_shapes.stm_list) (stm : 'meta Statement.t)
      (dest : 'meta target list) : 'meta target list Or_error.t =
    match path with
    | Insert index ->
        Tx.List.insert dest index (M.lift_stm stm)
    | In_stm (index, rest) ->
        Tx.List.With_errors.replace_m dest index ~f:(fun x ->
            Or_error.(M.insert_stm rest stm x >>| Option.some))

  let transform_stm (path : Path_shapes.stm_list)
      ~(f : 'meta Statement.t -> 'meta Statement.t Or_error.t)
      (dest : 'meta target list) : 'meta target list Or_error.t =
    match path with
    | In_stm (index, rest) ->
        Tx.List.With_errors.replace_m dest index ~f:(fun x ->
            Or_error.(M.transform_stm rest ~f x >>| Option.some))
    | Insert _ ->
        Or_error.error_s
          [%message
            "Can't use insert-at path to transform statements" ~here:[%here]
              ~path:(path : Path_shapes.stm_list)]

  let gen_insert_stm_on (index : int) (single_dest : 'meta target) :
      Path_shapes.stm_list Base_quickcheck.Generator.t list =
    let insert_after =
      Base_quickcheck.Generator.return (Path_shapes.insert (index + 1))
    in
    let insert_into =
      single_dest |> M.try_gen_insert_stm
      |> Option.map
           ~f:(Base_quickcheck.Generator.map ~f:(Path_shapes.in_stm index))
      |> Option.to_list
    in
    insert_after :: insert_into

  let gen_insert_stm (dest : 'meta target list) :
      Path_shapes.stm_list Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      ( Base_quickcheck.Generator.return (Path_shapes.insert 0)
      :: List.concat_mapi ~f:gen_insert_stm_on dest )
end

module Stm = Statement

module rec Statement :
  (Path_types.S_statement with type 'meta target = 'meta Stm.t) = struct
  type 'meta target = 'meta Stm.t

  let lift_stm = Fn.id

  let lower_stm = Fn.id

  let in_if_error (type meta) (dest : meta Stm.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here]
          ~target:(Stm.erase_meta dest : unit Stm.t)]

  let handle_in_if (dest : 'meta Stm.t)
      ~(f : 'meta Stm.If.t -> 'meta Stm.If.t Or_error.t) :
      'meta Stm.t Or_error.t =
    Stm.reduce dest
      ~if_stm:(fun ifs -> Or_error.(ifs |> f >>| Stm.if_stm))
      ~assign:(in_if_error dest) ~atomic_cmpxchg:(in_if_error dest)
      ~atomic_store:(in_if_error dest) ~nop:(in_if_error dest)

  let insert_stm (path : Path_shapes.stm) (stm : 'meta Stm.t)
      (dest : 'meta Stm.t) : 'meta Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.insert_stm rest stm) dest
    | _ ->
        Or_error.error_s
          [%message
            "Can't insert statement here" ~path:(path : Path_shapes.stm)]

  let transform_stm (path : Path_shapes.stm)
      ~(f : 'meta Stm.t -> 'meta Stm.t Or_error.t) (dest : 'meta Stm.t) :
      'meta Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.transform_stm rest ~f) dest
    | This_stm ->
        f dest

  let gen_if_stm_insert_stm (i : 'meta Stm.If.t) :
      Path_shapes.stm Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      (If_statement.gen_insert_stm i)
      ~f:Path_shapes.in_if

  let try_gen_insert_stm (type meta) (m : meta Stm.t) :
      Path_shapes.stm Base_quickcheck.Generator.t option =
    Stm.reduce m
      ~if_stm:(Fn.compose Option.some gen_if_stm_insert_stm)
      ~assign:(Fn.const None) ~atomic_cmpxchg:(Fn.const None)
      ~atomic_store:(Fn.const None) ~nop:(Fn.const None)
end

and Statement_list :
  (Path_types.S_statement_list with type 'meta target = 'meta Stm.t) =
  Make_statement_list (Statement)

and If_statement :
  (Path_types.S_if_statement with type 'meta target = 'meta Stm.If.t) =
struct
  type 'meta target = 'meta Stm.If.t

  module B = Stm.If.Base_map (Or_error)
  module Block_stms = Block.On_meta_statement_list (Stm)
  module Block_stms_err = Block_stms.On_monad (Or_error)

  (* TODO(@MattWindsor91): this will probably need to be changed to accept
     full blocks. *)

  let handle_stm (type meta) (path : Path_shapes.ifs)
      ~(f :
            Path_shapes.stm_list
         -> meta Stm.t list
         -> meta Stm.t list Or_error.t) (ifs : meta Stm.If.t) :
      meta Stm.If.t Or_error.t =
    let lift_f rest = Block_stms_err.map_m ~f:(f rest) in
    match path with
    | In_block (branch, rest) ->
        let t_branch, f_branch =
          ( (if branch then lift_f rest else Or_error.return)
          , if branch then Or_error.return else lift_f rest )
        in
        B.bmap ifs ~cond:Or_error.return ~t_branch ~f_branch
    | This_cond ->
        Or_error.error_string "Not a statement path"

  let insert_stm (path : Path_shapes.ifs) (stm : 'meta Stm.t) :
      'meta Stm.If.t -> 'meta Stm.If.t Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : Path_shapes.ifs)
      ~(f : 'meta Stm.t -> 'meta Stm.t Or_error.t) :
      'meta Stm.If.t -> 'meta Stm.If.t Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)

  let gen_insert_stm_for_branch (branch : bool)
      (branch_block : ('meta, 'meta Stm.t) Block.t) :
      Path_shapes.ifs Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      ~f:(Path_shapes.in_block branch)
      (Statement_list.gen_insert_stm (Block.statements branch_block))

  let gen_insert_stm (ifs : 'meta Stm.If.t) :
      Path_shapes.ifs Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      [ gen_insert_stm_for_branch true (Stm.If.t_branch ifs)
      ; gen_insert_stm_for_branch false (Stm.If.f_branch ifs) ]
end

let%test_unit "insertions into an empty list are always at index 0" =
  Base_quickcheck.Test.run_exn
    ( module struct
      type t = Path_shapes.stm_list [@@deriving sexp]

      let quickcheck_generator = Statement_list.gen_insert_stm []

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end )
    ~f:(function
      | Path_shapes.Insert 0 -> () | _ -> failwith "Unexpected path")

module Fun = Function

module Function :
  Path_types.S_function with type 'meta target := 'meta Function.t = struct
  type 'meta target = 'meta Function.t

  let gen_insert_stm (func : 'meta target) :
      Path_shapes.func Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      (Statement_list.gen_insert_stm (Function.body_stms func))
      ~f:Path_shapes.in_stms

  let handle_stm (path : Path_shapes.func)
      ~(f :
            Path_shapes.stm_list
         -> 'meta Stm.t list
         -> 'meta Stm.t list Or_error.t) (func : 'meta Function.t) :
      'meta Function.t Or_error.t =
    match path with
    | In_stms rest ->
        Or_error.(
          f rest (Function.body_stms func) >>| Function.with_body_stms func)

  let insert_stm (path : Path_shapes.func) (stm : 'meta Stm.t) :
      'meta target -> 'meta target Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : Path_shapes.func)
      ~(f : 'meta Stm.t -> 'meta Stm.t Or_error.t) :
      'meta target -> 'meta target Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)
end

module Program :
  Path_types.S_program with type 'meta target := 'meta Program.t = struct
  type 'meta target = 'meta Program.t

  let gen_insert_stm (prog : 'meta target) :
      Path_shapes.program Base_quickcheck.Generator.t =
    let prog_gens =
      List.mapi (Program.functions prog) ~f:(fun index (_, func) ->
          Base_quickcheck.Generator.map
            (Function.gen_insert_stm func)
            ~f:(Path_shapes.in_func index))
    in
    Base_quickcheck.Generator.union prog_gens

  let handle_stm (path : Path_shapes.program)
      ~(f : Path_shapes.func -> 'meta Fun.t -> 'meta Fun.t Or_error.t)
      (prog : 'meta target) : 'meta target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | In_func (index, rest) ->
        let functions = Program.functions prog in
        let%map functions' =
          Tx.List.With_errors.replace_m functions index
            ~f:(fun (name, func) ->
              let%map func' = f rest func in
              Some (name, func'))
        in
        Program.with_functions prog functions'

  let insert_stm (path : Path_shapes.program) (stm : 'meta Stm.t) :
      'meta target -> 'meta target Or_error.t =
    handle_stm path ~f:(fun rest -> Function.insert_stm rest stm)

  let transform_stm (path : Path_shapes.program)
      ~(f : 'meta Stm.t -> 'meta Stm.t Or_error.t) :
      'meta target -> 'meta target Or_error.t =
    handle_stm path ~f:(Function.transform_stm ~f)
end
