(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Fqueue *)

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Tx = Travesty_base_exts
  module Named = Ac.C_named
end

(** [sift_decls maybe_decl_list] tries to separate [maybe_decl_list] into a
    list of declarations followed immediately by a list of code, C89-style. *)
let sift_decls (maybe_decl_list : ([> `Decl of 'd] as 'a) list) :
    ('d list * 'a list) Or_error.t =
  Or_error.(
    Tx.List.With_errors.fold_m maybe_decl_list
      ~init:(Fqueue.empty, Fqueue.empty) ~f:(fun (decls, rest) -> function
      | `Decl d ->
          if Fqueue.is_empty rest then return (Fqueue.enqueue decls d, rest)
          else error_string "Declarations must go before code."
      | item ->
          return (decls, Fqueue.enqueue rest item))
    >>| fun (decls, rest) -> (Fqueue.to_list decls, Fqueue.to_list rest))

(** [ensure_functions xs] makes sure that each member of [xs] is a function
    definition. *)
let ensure_functions :
    Ast.External_decl.t list -> Ast.Function_def.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Fun f ->
        Or_error.return f
    | d ->
        Or_error.error_s
          [%message "Expected a function" ~got:(d : Ast.External_decl.t)])

(** [ensure_statements xs] makes sure that each member of [xs] is a
    statement. *)
let ensure_statements :
    Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Stm f ->
        Or_error.return f
    | d ->
        Or_error.error_s
          [%message "Expected a statement" ~got:(d : Ast.Compound_stm.Elt.t)])

let validate_func_void_type (f : Ast.Function_def.t) : Validate.t =
  match f.decl_specs with
  | [`Void] ->
      Validate.pass
  | xs ->
      Validate.fail_s
        [%message "Expected 'void'" ~got:(xs : Ast.Decl_spec.t list)]

let validate_func_no_knr : Ast.Function_def.t Validate.check =
  Validate.booltest
    (fun f -> List.is_empty f.Ast.Function_def.decls)
    ~if_false:"K&R style function definitions not supported"

let validate_func : Ast.Function_def.t Validate.check =
  Validate.all [validate_func_void_type; validate_func_no_knr]

let param_type_list :
    Ast.Param_type_list.t -> Fir.Type.t Named.Alist.t Or_error.t = function
  | {style= `Variadic; _} ->
      Or_error.error_string "Variadic arguments not supported"
  | {style= `Normal; params} ->
      Or_error.(
        params
        |> Tx.Or_error.combine_map ~f:Abstract_prim.param_decl
        >>| Named.alist_of_list)

let func_signature :
       Ast.Declarator.t
    -> (Act_common.C_id.t * Fir.Type.t Named.Alist.t) Or_error.t = function
  | {pointer= Some _; _} ->
      Or_error.error_string "Pointers not supported yet"
  | {pointer= None; direct= Fun_decl (Id name, param_list)} ->
      Or_error.(param_list |> param_type_list >>| Tuple2.create name)
  | x ->
      Or_error.error_s
        [%message
          "Unsupported function declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)]

let model_atomic_cmpxchg_expr (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_cmpxchg ~expr
    >>| Fir.Expression.atomic_cmpxchg)

let model_atomic_fetch_expr (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fetch ~expr ~op
    >>| Fir.Expression.atomic_fetch)

let model_atomic_load_expr (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Or_error.t =
  ignore expr ;
  Or_error.(
    args |> Abstract_atomic.model_load >>| Fir.Expression.atomic_load)

let fetch_call_alist (modeller : Ast.Expr.t list -> op:Fir.Op.Fetch.t -> 'a)
    : (Ac.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t =
  List.map (Fir.Op.Fetch.all_list ()) ~f:(fun op ->
      let name = Ac.C_id.of_string (Abstract_atomic.fetch_name op) in
      (name, modeller ~op))

let expr_call_table :
    (   Ast.Expr.t list
     -> expr:(Ast.Expr.t -> Fir.Expression.t Or_error.t)
     -> Fir.Expression.t Or_error.t)
    Map.M(Ac.C_id).t
    Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       ( fetch_call_alist model_atomic_fetch_expr
       @ [ ( Ac.C_id.of_string Abstract_atomic.cmpxchg_name
           , model_atomic_cmpxchg_expr )
         ; ( Ac.C_id.of_string Abstract_atomic.load_name
           , model_atomic_load_expr ) ] ))

let expr_call_handler (func_name : Ac.C_id.t) :
    (   Ast.Expr.t list
     -> expr:(Ast.Expr.t -> Fir.Expression.t Or_error.t)
     -> Fir.Expression.t Or_error.t)
    Or_error.t =
  func_name
  |> Map.find (Lazy.force expr_call_table)
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message
              "Unsupported function in expression position"
                ~got:(func_name : Ac.C_id.t)])

let function_call (func : Ast.Expr.t) (arguments : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind func_name = Abstract_prim.expr_to_identifier func in
    let%bind call_handler = expr_call_handler func_name in
    call_handler arguments ~expr)

let identifier_to_expr (id : Ac.C_id.t) : Fir.Expression.t =
  match Abstract_prim.identifier_to_constant id with
  | Some k ->
      Fir.Expression.constant k
  | None ->
      Fir.Expression.variable id

let bop : Operators.Bin.t -> Fir.Op.Binary.t Or_error.t =
  Or_error.(
    function
    | `Add ->
        return Fir.Op.Binary.add
    | `Eq ->
        return Fir.Op.Binary.eq
    | `Land ->
        return Fir.Op.Binary.l_and
    | `Lor ->
        return Fir.Op.Binary.l_or
    | `Sub ->
        return Fir.Op.Binary.sub
    | `And ->
        return Fir.Op.Binary.b_and
    | `Or ->
        return Fir.Op.Binary.b_or
    | `Xor ->
        return Fir.Op.Binary.b_xor
    | op ->
        error_s
          [%message
            "Unsupported binary operator" ~got:(op : Operators.Bin.t)])

let prefix_op : Operators.Pre.t -> Fir.Op.Unary.t Or_error.t =
  Or_error.(
    function
    | `Lnot ->
        return Fir.Op.Unary.l_not
    | op ->
        error_s
          [%message
            "Unsupported prefix operator" ~got:(op : Operators.Pre.t)])

let rec expr : Ast.Expr.t -> Fir.Expression.t Or_error.t =
  Or_error.Let_syntax.(
    let model_binary l op r =
      let%map l' = expr l and r' = expr r and op' = bop op in
      Fir.Expression.bop op' l' r'
    in
    let model_prefix op x =
      let%map x' = expr x and op' = prefix_op op in
      Fir.Expression.uop op' x'
    in
    function
    | Brackets e ->
        expr e
    | Binary (l, op, r) ->
        model_binary l op r
    | Constant k ->
        Or_error.map ~f:Fir.Expression.constant (Abstract_prim.constant k)
    | Identifier id ->
        Ok (identifier_to_expr id)
    | Prefix (`Deref, expr) ->
        Or_error.(
          expr |> Abstract_prim.expr_to_lvalue >>| Fir.Lvalue.deref
          >>| Fir.Expression.lvalue)
    | Prefix (op, expr) ->
        model_prefix op expr
    | Call {func; arguments} ->
        function_call func arguments ~expr
    | ( Postfix _
      | Ternary _
      | Cast _
      | Subscript _
      | Field _
      | Sizeof_type _
      | String _ ) as e ->
        Or_error.error_s
          [%message "Unsupported expression" ~got:(e : Ast.Expr.t)])

let prim : Fir.Prim_statement.t -> unit Fir.Statement.t =
  Fir.Statement.prim ()

let model_atomic_cmpxchg_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_cmpxchg ~expr
    >>| Fir.Prim_statement.atomic_cmpxchg >>| prim)

let model_atomic_fence_stm (args : Ast.Expr.t list)
    ~(mode : Fir.Atomic_fence.Mode.t) : unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fence ~mode
    >>| Fir.Prim_statement.atomic_fence >>| prim)

let fence_call_alist :
    ( Ac.C_id.t
    , Ast.Expr.t list -> unit Fir.Statement.t Or_error.t )
    List.Assoc.t
    Lazy.t =
  lazy
    (List.map (Fir.Atomic_fence.Mode.all_list ()) ~f:(fun mode ->
         let name = Ac.C_id.of_string (Abstract_atomic.fence_name mode) in
         (name, model_atomic_fence_stm ~mode)))

let model_atomic_fetch_stm (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fetch ~expr ~op
    >>| Fir.Prim_statement.atomic_fetch >>| prim)

let model_atomic_store_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_store ~expr
    >>| Fir.Prim_statement.atomic_store >>| prim)

let model_atomic_xchg_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_xchg ~expr
    >>| Fir.Prim_statement.atomic_xchg >>| prim)

let expr_stm_call_table :
    (Ast.Expr.t list -> unit Fir.Statement.t Or_error.t) Map.M(Ac.C_id).t
    Lazy.t =
  Lazy.Let_syntax.(
    let%map fence_call_alist = fence_call_alist in
    Map.of_alist_exn
      (module Ac.C_id)
      ( fetch_call_alist model_atomic_fetch_stm
      @ fence_call_alist
      @ [ ( Ac.C_id.of_string Abstract_atomic.cmpxchg_name
          , model_atomic_cmpxchg_stm )
        ; ( Ac.C_id.of_string Abstract_atomic.store_name
          , model_atomic_store_stm )
        ; (Ac.C_id.of_string Abstract_atomic.xchg_name, model_atomic_xchg_stm)
        ] ))

let arbitrary_procedure_call (function_id : Ac.C_id.t)
    (raw_arguments : Ast.Expr.t list) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map arguments = Tx.Or_error.combine_map ~f:expr raw_arguments in
    let call = Fir.Call.make ~arguments ~function_id () in
    call |> Fir.Prim_statement.procedure_call |> prim)

let procedure_call (func : Ast.Expr.t) (arguments : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind function_id = Abstract_prim.expr_to_identifier func in
    match Map.find (Lazy.force expr_stm_call_table) function_id with
    | Some call_handler ->
        call_handler arguments
    | None ->
        arbitrary_procedure_call function_id arguments)

let expr_stm : Ast.Expr.t -> unit Fir.Statement.t Or_error.t = function
  | Binary (l, `Assign, r) ->
      Or_error.Let_syntax.(
        let%map lvalue = Abstract_prim.expr_to_lvalue l
        and rvalue = expr r in
        let assign = Fir.Assign.make ~lvalue ~rvalue in
        assign |> Fir.Prim_statement.assign |> prim)
  | Call {func; arguments} ->
      procedure_call func arguments
  | ( Brackets _
    | Constant _
    | Prefix _
    | Postfix _
    | Binary _
    | Ternary _
    | Cast _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _
    | Identifier _ ) as e ->
      Or_error.error_s
        [%message "Unsupported expression statement" ~got:(e : Ast.Expr.t)]

let possible_compound_to_list : Ast.Stm.t -> Ast.Stm.t list Or_error.t =
  function
  | Ast.Stm.Compound elems ->
      let open Or_error.Let_syntax in
      let%bind _, ast_nondecls = sift_decls elems in
      ensure_statements ast_nondecls
  | stm ->
      Or_error.return [stm]

(** Type of recursive statement converters. *)
type mu_stm = Ast.Stm.t -> unit Fir.Statement.t Or_error.t

let block_list (model_stm : mu_stm) (old_block : Ast.Stm.t list) :
    (unit, unit Fir.Statement.t) Fir.Block.t Or_error.t =
  Or_error.(
    Tx.Or_error.combine_map ~f:model_stm old_block
    >>| Fir.Block.of_statement_list)

let block (model_stm : mu_stm) (old_block : Ast.Stm.t) :
    (unit, unit Fir.Statement.t) Fir.Block.t Or_error.t =
  Or_error.(old_block |> possible_compound_to_list >>= block_list model_stm)

let model_if (model_stm : mu_stm) (old_cond : Ast.Expr.t)
    (old_t_branch : Ast.Stm.t) (old_f_branch : Ast.Stm.t option) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind cond = expr old_cond in
    let%bind t_branch = block model_stm old_t_branch in
    let%map f_branch =
      Option.value_map old_f_branch ~f:(block model_stm)
        ~default:(Ok (Fir.Block.of_statement_list []))
    in
    let ifs = Fir.If.make ~cond ~t_branch ~f_branch in
    Fir.Statement.if_stm ifs)

let loop (model_stm : mu_stm) (old_cond : Ast.Expr.t) (old_body : Ast.Stm.t)
    (kind : Fir.Flow_block.While.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map cond = expr old_cond and body = block model_stm old_body in
    Fir.Statement.flow (Fir.Flow_block.while_loop ~cond ~body ~kind))

let lock (model_stm : mu_stm) (old_body : Ast.Compound_stm.t)
    (kind : Fir.Flow_block.Lock.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): we should really support declarations here. *)
    let%bind ast_stms = ensure_statements old_body in
    let%map body = block_list model_stm ast_stms in
    Fir.Statement.flow (Fir.Flow_block.lock_block ~body ~kind))

let rec stm : Ast.Stm.t -> unit Fir.Statement.t Or_error.t = function
  | Expr None ->
      Ok (prim Fir.Prim_statement.nop)
  | Expr (Some e) ->
      expr_stm e
  | If {cond; t_branch; f_branch} ->
      model_if stm cond t_branch f_branch
  | Continue ->
      Ok (prim Fir.Prim_statement.continue)
  | Break ->
      Ok (prim Fir.Prim_statement.break)
  | Return None ->
      Ok (prim Fir.Prim_statement.return)
  | Return (Some _) as s ->
      Or_error.error_s
        [%message "Value returns not supported in FIR" ~got:(s : Ast.Stm.t)]
  | Atomic b ->
      lock stm b Atomic
  | Synchronized b ->
      lock stm b Synchronized
  | While (c, b) ->
      loop stm c b While
  | Do_while (b, c) ->
      loop stm c b Do_while
  | Label (Normal l, Expr None) ->
      (* This is a particularly weird subset of the labels, but I'm not sure
         how best to expand it. *)
      Ok (prim (Fir.Prim_statement.label l))
  | Goto l ->
      Ok (prim (Fir.Prim_statement.goto l))
  | (Label _ | Compound _ | Switch _ | For _) as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]

let func_body (body : Ast.Compound_stm.t) :
    (Fir.Initialiser.t Named.Alist.t * unit Fir.Statement.t list) Or_error.t
    =
  Or_error.Let_syntax.(
    let%bind ast_decls, ast_nondecls = sift_decls body in
    let%bind ast_stms = ensure_statements ast_nondecls in
    let%map decls = Tx.Or_error.combine_map ~f:Abstract_prim.decl ast_decls
    and stms = Tx.Or_error.combine_map ~f:stm ast_stms in
    (Named.alist_of_list decls, stms))

let func (f : Ast.Function_def.t) : unit Fir.Function.t Named.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind () = Validate.result (validate_func f) in
    let%map name, parameters = func_signature f.signature
    and body_decls, body_stms = func_body f.body in
    Named.make ~name
      (Fir.Function.make ~parameters ~body_decls ~body_stms ()))

let translation_unit (prog : Ast.Translation_unit.t) :
    unit Fir.Program.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind ast_decls, ast_nondecls = sift_decls prog in
    let%bind ast_funs = ensure_functions ast_nondecls in
    let%map global_list =
      Tx.Or_error.combine_map ~f:Abstract_prim.decl ast_decls
    and function_list = Tx.Or_error.combine_map ~f:func ast_funs in
    let globals = Named.alist_of_list global_list in
    let functions = Named.alist_of_list function_list in
    Fir.Program.make ~globals ~functions)

module Litmus_conv = Act_litmus.Convert.Make (struct
  module From = struct
    include Ast.Litmus
    module Lang = Ast.Litmus_lang
  end

  module To = Fir.Litmus.Test

  let program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t = func

  let constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t =
    Abstract_prim.constant
end)

let litmus_post :
       Ast_basic.Constant.t Act_litmus.Postcondition.t
    -> Fir.Constant.t Act_litmus.Postcondition.t Or_error.t =
  Litmus_conv.convert_post

let litmus : Ast.Litmus.t -> Fir.Litmus.Test.t Or_error.t =
  Litmus_conv.convert

let litmus_of_raw_ast (ast : Act_litmus.Ast.M(Ast.Litmus_lang).t) =
  Or_error.(ast |> Ast.Litmus.of_ast >>= litmus)
