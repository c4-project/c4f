(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Tx = Travesty_base_exts
end

let ensure_statements :
    Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Stm f ->
        Or_error.return f
    | d ->
        Or_error.error_s
          [%message "Expected a statement" ~got:(d : Ast.Compound_stm.Elt.t)])

let expr = Abstract_expr.model

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
  lazy
    Abstract_atomic.(
      Map.of_alist_exn
        (module Ac.C_id)
        ( fetch_call_alist model_atomic_fetch_stm
        @ fence_call_alist model_atomic_fence_stm
        @ [ (Ac.C_id.of_string cmpxchg_name, model_atomic_cmpxchg_stm)
          ; (Ac.C_id.of_string store_name, model_atomic_store_stm)
          ; (Ac.C_id.of_string xchg_name, model_atomic_xchg_stm) ] ))

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
      Or_error.Let_syntax.(
        let%bind _, ast_nondecls = Abstract_prim.sift_decls elems in
        ensure_statements ast_nondecls)
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

let rec model : Ast.Stm.t -> unit Fir.Statement.t Or_error.t = function
  | Expr None ->
      Ok (prim Fir.Prim_statement.nop)
  | Expr (Some e) ->
      expr_stm e
  | If {cond; t_branch; f_branch} ->
      model_if model cond t_branch f_branch
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
      lock model b Atomic
  | Synchronized b ->
      lock model b Synchronized
  | While (c, b) ->
      loop model c b While
  | Do_while (b, c) ->
      loop model c b Do_while
  | Label (Normal l, Expr None) ->
      (* This is a particularly weird subset of the labels, but I'm not sure
         how best to expand it. *)
      Ok (prim (Fir.Prim_statement.label l))
  | Goto l ->
      Ok (prim (Fir.Prim_statement.goto l))
  | (Label _ | Compound _ | Switch _ | For _) as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]
