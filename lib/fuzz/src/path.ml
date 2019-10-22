(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Tx = Travesty_base_exts
module Stm = Act_c_mini.Statement
module Fun = Act_c_mini.Function
module Prog = Act_c_mini.Program

module rec Statement :
  (Path_types.S_statement with type target = Metadata.t Stm.t) = struct
  type target = Metadata.t Stm.t

  let in_if_error (dest : Metadata.t Stm.t) (_ : 'a) : 'b Or_error.t =
    Or_error.error_s
      [%message
        "Invalid target for 'in_if' path" [%here]
          ~target:(Stm.erase_meta dest : unit Stm.t)]

  let handle_in_if (dest : Metadata.t Stm.t)
      ~(f : Metadata.t Stm.If.t -> Metadata.t Stm.If.t Or_error.t) :
      Metadata.t Stm.t Or_error.t =
    Stm.reduce dest
      ~if_stm:(fun ifs -> Or_error.(ifs |> f >>| Stm.if_stm))
      ~assign:(in_if_error dest) ~atomic_cmpxchg:(in_if_error dest)
      ~atomic_store:(in_if_error dest) ~nop:(in_if_error dest)

  let insert_stm (path : Path_shapes.stm) (stm : Metadata.t Stm.t)
      (dest : Metadata.t Stm.t) : Metadata.t Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.insert_stm rest stm) dest
    | This_stm ->
        Or_error.error_s
          [%message
            "Can't insert statement here" ~path:(path : Path_shapes.stm)]

  let transform_stm (path : Path_shapes.stm)
      ~(f : Metadata.t Stm.t -> Metadata.t Stm.t Or_error.t)
      (dest : Metadata.t Stm.t) : Metadata.t Stm.t Or_error.t =
    match path with
    | In_if rest ->
        handle_in_if ~f:(If_statement.transform_stm rest ~f) dest
    | This_stm ->
        f dest

  let gen_if_stm_insert_stm (i : Metadata.t Stm.If.t) :
      Path_shapes.stm Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      (If_statement.gen_insert_stm i)
      ~f:Path_shapes.in_if

  let try_gen_insert_stm (m : Metadata.t Stm.t) :
      Path_shapes.stm Base_quickcheck.Generator.t option =
    Stm.reduce m
      ~if_stm:(Fn.compose Option.some gen_if_stm_insert_stm)
      ~assign:(Fn.const None) ~atomic_cmpxchg:(Fn.const None)
      ~atomic_store:(Fn.const None) ~nop:(Fn.const None)
end

and Statement_list :
  (Path_types.S_statement_list with type target = Metadata.t Stm.t) = struct
  module M = Statement

  type target = M.target

  let handle_in_stm (dest : target list) (index : int)
      ~(f : target -> target Or_error.t) : target list Or_error.t =
    Tx.List.With_errors.replace_m dest index ~f:(fun x ->
        Or_error.(x |> f >>| Option.some))

  let bad_stm_list_path_error (path : Path_shapes.stm_list)
      ~(here : Source_code_position.t) ~(context : string) :
      target list Or_error.t =
    Or_error.error_s
      [%message
        "Can't use this statement-list path here"
          ~here:(here : Source_code_position.t)
          ~context
          ~path:(path : Path_shapes.stm_list)]

  let insert_stm (path : Path_shapes.stm_list) (stm : Metadata.t Stm.t)
      (dest : target list) : target list Or_error.t =
    match path with
    | Insert index ->
        Tx.List.insert dest index stm
    | In_stm (index, rest) ->
        handle_in_stm dest index ~f:(M.insert_stm rest stm)
    | On_stm_range (_, _) ->
        bad_stm_list_path_error path ~context:"insert_stm" ~here:[%here]

  let transform_stm (path : Path_shapes.stm_list)
      ~(f : Metadata.t Stm.t -> Metadata.t Stm.t Or_error.t)
      (dest : target list) : target list Or_error.t =
    match path with
    | In_stm (index, rest) ->
        handle_in_stm dest index ~f:(M.transform_stm rest ~f)
    | On_stm_range (_index, _length) ->
        Or_error.unimplemented "TODO"
    | Insert _ ->
        bad_stm_list_path_error path ~context:"transform_stm" ~here:[%here]

  let gen_insert_stm_on (index : int) (single_dest : target) :
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

  let gen_insert_stm (dest : target list) :
      Path_shapes.stm_list Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      ( Base_quickcheck.Generator.return (Path_shapes.insert 0)
      :: List.concat_mapi ~f:gen_insert_stm_on dest )
end

and If_statement :
  (Path_types.S_if_statement with type target = Metadata.t Stm.If.t) =
struct
  type target = Metadata.t Stm.If.t

  module B = Stm.If.Base_map (Or_error)
  module Block_stms = Act_c_mini.Block.On_meta_statement_list (Stm)
  module Block_stms_err = Block_stms.On_monad (Or_error)

  (* TODO(@MattWindsor91): this will probably need to be changed to accept
     full blocks. *)

  let handle_stm (path : Path_shapes.ifs)
      ~(f :
            Path_shapes.stm_list
         -> Metadata.t Stm.t list
         -> Metadata.t Stm.t list Or_error.t) (ifs : Metadata.t Stm.If.t) :
      Metadata.t Stm.If.t Or_error.t =
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

  let insert_stm (path : Path_shapes.ifs) (stm : Metadata.t Stm.t) :
      Metadata.t Stm.If.t -> Metadata.t Stm.If.t Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : Path_shapes.ifs)
      ~(f : Metadata.t Stm.t -> Metadata.t Stm.t Or_error.t) :
      Metadata.t Stm.If.t -> Metadata.t Stm.If.t Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)

  let gen_insert_stm_for_branch (branch : bool)
      (branch_block : (Metadata.t, Metadata.t Stm.t) Act_c_mini.Block.t) :
      Path_shapes.ifs Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      ~f:(Path_shapes.in_block branch)
      (Statement_list.gen_insert_stm
         (Act_c_mini.Block.statements branch_block))

  let gen_insert_stm (ifs : Metadata.t Stm.If.t) :
      Path_shapes.ifs Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.union
      [ gen_insert_stm_for_branch true (Stm.If.t_branch ifs)
      ; gen_insert_stm_for_branch false (Stm.If.f_branch ifs) ]
end

module Function :
  Path_types.S_function with type target := Metadata.t Fun.t = struct
  type target = Metadata.t Fun.t

  let gen_insert_stm (func : target) :
      Path_shapes.func Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      (Statement_list.gen_insert_stm (Fun.body_stms func))
      ~f:Path_shapes.in_stms

  let handle_stm (path : Path_shapes.func)
      ~(f :
            Path_shapes.stm_list
         -> Metadata.t Stm.t list
         -> Metadata.t Stm.t list Or_error.t) (func : Metadata.t Fun.t) :
      Metadata.t Fun.t Or_error.t =
    match path with
    | In_stms rest ->
        Or_error.(f rest (Fun.body_stms func) >>| Fun.with_body_stms func)

  let insert_stm (path : Path_shapes.func) (stm : Metadata.t Stm.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Statement_list.insert_stm rest stm)

  let transform_stm (path : Path_shapes.func)
      ~(f : Metadata.t Stm.t -> Metadata.t Stm.t Or_error.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(Statement_list.transform_stm ~f)
end

module Program :
  Path_types.S_program with type target := Metadata.t Prog.t = struct
  type target = Metadata.t Prog.t

  let gen_insert_stm (prog : target) :
      Path_shapes.program Base_quickcheck.Generator.t =
    let prog_gens =
      List.mapi (Prog.functions prog) ~f:(fun index (_, func) ->
          Base_quickcheck.Generator.map
            (Function.gen_insert_stm func)
            ~f:(Path_shapes.in_func index))
    in
    Base_quickcheck.Generator.union prog_gens

  let handle_stm (path : Path_shapes.program)
      ~(f :
         Path_shapes.func -> Metadata.t Fun.t -> Metadata.t Fun.t Or_error.t)
      (prog : target) : target Or_error.t =
    let open Or_error.Let_syntax in
    match path with
    | In_func (index, rest) ->
        let functions = Prog.functions prog in
        let%map functions' =
          Tx.List.With_errors.replace_m functions index
            ~f:(fun (name, func) ->
              let%map func' = f rest func in
              Some (name, func'))
        in
        Prog.with_functions prog functions'

  let insert_stm (path : Path_shapes.program) (stm : Metadata.t Stm.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(fun rest -> Function.insert_stm rest stm)

  let transform_stm (path : Path_shapes.program)
      ~(f : Metadata.t Stm.t -> Metadata.t Stm.t Or_error.t) :
      target -> target Or_error.t =
    handle_stm path ~f:(Function.transform_stm ~f)
end
