(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor_base
  module Ac = Act_common
  module Fir = Act_fir
  module Tx = Travesty_base_exts
end

let ensure_statements :
    Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Stm f ->
        Ok f
    | d ->
        Or_error.error_s
          [%message "Expected a statement" ~got:(d : Ast.Compound_stm.Elt.t)])

let expr = Abstract_expr.model

let model_atomic_cmpxchg_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_cmpxchg ~expr
    >>| A.(
          construct
            Fir.(
              Statement.prim' @> Prim_statement.atomic
              @> Atomic_statement.cmpxchg)))

let model_atomic_fence_stm (args : Ast.Expr.t list)
    ~(mode : Fir.Atomic_fence.Mode.t) : unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fence ~mode
    >>| A.(
          construct
            Fir.(
              Statement.prim' @> Prim_statement.atomic
              @> Atomic_statement.fence)))

let model_atomic_fetch_stm (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fetch ~expr ~op
    >>| A.(
          construct
            Fir.(
              Statement.prim' @> Prim_statement.atomic
              @> Atomic_statement.fetch)))

let model_atomic_store_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_store ~expr
    >>| A.(
          construct
            Fir.(
              Statement.prim' @> Prim_statement.atomic
              @> Atomic_statement.store)))

let model_atomic_xchg_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_xchg ~expr
    >>| A.(
          construct
            Fir.(
              Statement.prim' @> Prim_statement.atomic
              @> Atomic_statement.xchg)))

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
    A.(construct Fir.(Statement.prim' @> Prim_statement.procedure_call) call))

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
        A.(construct Fir.(Statement.prim' @> Prim_statement.assign) assign))
  | Call {func; arguments} ->
      procedure_call func arguments
  | ( Brackets _ (* should've been debracketed already *)
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
      (* We don't support inner declarations at the moment. *)
      ensure_statements elems
  | stm ->
      Ok [stm]

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
    A.construct Fir.Statement.if_stm ifs)

let for_loop_lvalue_unify (ilv : Fir.Lvalue.t) (clv : Fir.Lvalue.t)
    (ulv : Fir.Lvalue.t) : Fir.Lvalue.t Or_error.t =
  if not ([%equal: Fir.Lvalue.t] ilv clv) then
    Or_error.error_s
      [%message
        "Init and comparison lvalues differ; not supported in FIR"
          ~init:(ilv : Fir.Lvalue.t)
          ~comparison:(clv : Fir.Lvalue.t)]
  else if not ([%equal: Fir.Lvalue.t] ilv ulv) then
    Or_error.error_s
      [%message
        "Init and update lvalues differ; not supported in FIR"
          ~init:(ilv : Fir.Lvalue.t)
          ~update:(ulv : Fir.Lvalue.t)]
  else Ok ilv

let for_loop_lvalue (ilv : Ast.Expr.t) (clv : Ast.Expr.t) (ulv : Ast.Expr.t)
    : Fir.Lvalue.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind ilv' = Abstract_prim.expr_to_lvalue ilv in
    let%bind clv' = Abstract_prim.expr_to_lvalue clv in
    let%bind ulv' = Abstract_prim.expr_to_lvalue ulv in
    for_loop_lvalue_unify ilv' clv' ulv')

let for_loop_direction (cop : Operators.Bin.t) (uop : Operators.Post.t) :
    Fir.Flow_block.For.Direction.t Or_error.t =
  match (cop, uop) with
  | `Lt, `Inc | `Ne, `Inc ->
      Ok Up_exclusive
  | `Lt, _ ->
      Or_error.error_string "if comparison op is <, update must be ++"
  | `Le, `Inc ->
      Ok Up_inclusive
  | `Le, _ ->
      Or_error.error_string "if comparison op is <=, update must be ++"
  | `Gt, `Dec | `Ne, `Dec ->
      Ok Down_exclusive
  | `Gt, _ ->
      Or_error.error_string "if comparison op is >, update must be --"
  | `Ge, `Dec ->
      Ok Down_inclusive
  | `Ge, _ ->
      Or_error.error_string "if comparison op is >=, update must be --"
  | _, _ ->
      Or_error.error_s
        [%message
          "unsupported comparison/update operators"
            ~comparison:(cop : Operators.Bin.t)
            ~update:(uop : Operators.Post.t)]

let for_loop_structured (model_stm : mu_stm) (ilv : Ast.Expr.t)
    (clv : Ast.Expr.t) (ulv : Ast.Expr.t) (init : Ast.Expr.t)
    (cmp : Ast.Expr.t) (cop : Operators.Bin.t) (uop : Operators.Post.t)
    (body : Ast.Stm.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind lvalue = for_loop_lvalue ilv clv ulv in
    let%bind direction = for_loop_direction cop uop in
    let%bind init_value = expr init in
    let%bind cmp_value = expr cmp in
    let%map body = block model_stm body in
    let control =
      Fir.Flow_block.For.make ~lvalue ~init_value ~cmp_value ~direction
    in
    A.construct Fir.Statement.flow (Fir.Flow_block.for_loop ~control ~body))

let rec debracket (x : Ast.Expr.t) : Ast.Expr.t =
  (* We do this inline in the abstraction for ordinary expressions, so this
     purely exists for statement/for-loop component expressions. *)
  match x with Brackets x -> debracket x | y -> y

let for_loop (model_stm : mu_stm) (old_init_opt : Ast.Expr.t option)
    (old_cond_opt : Ast.Expr.t option) (old_update_opt : Ast.Expr.t option)
    (body : Ast.Stm.t) : unit Fir.Statement.t Or_error.t =
  match
    ( Option.map ~f:debracket old_init_opt
    , Option.map ~f:debracket old_cond_opt
    , Option.map ~f:debracket old_update_opt )
  with
  | ( Some (Binary (ilv, `Assign, init))
    , Some (Binary (clv, cop, cmp))
    , Some (Postfix (ulv, uop)) ) ->
      for_loop_structured model_stm ilv clv ulv init cmp cop uop body
  | None, _, _ ->
      Or_error.error_string "FIR for loops must have an initialiser"
  | _, None, _ ->
      Or_error.error_string "FIR for loops must have a condition"
  | _, _, None ->
      Or_error.error_string "FIR for loops must have an updater"
  | Some (Binary (_, `Assign, _)), Some (Binary _), Some _ ->
      Or_error.error_string
        "Fir for loop updaters must be of the form (x++) or (x--)"
  | Some (Binary (_, `Assign, _)), Some _, Some _ ->
      Or_error.error_string
        "Fir for loop conditions must be of the form (x OP y)"
  | Some _, Some _, Some _ ->
      Or_error.error_string
        "Fir for loop initialisers must be of the form (x = y)"

let while_loop (model_stm : mu_stm) (old_cond : Ast.Expr.t)
    (old_body : Ast.Stm.t) (kind : Fir.Flow_block.While.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map cond = expr old_cond and body = block model_stm old_body in
    A.construct Fir.Statement.flow
      (Fir.Flow_block.while_loop ~cond ~body ~kind))

let lock (model_stm : mu_stm) (old_body : Ast.Compound_stm.t)
    (kind : Fir.Flow_block.Lock.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): we should really support declarations here. *)
    let%bind ast_stms = ensure_statements old_body in
    let%map body = block_list model_stm ast_stms in
    A.construct Fir.Statement.flow (Fir.Flow_block.lock_block ~body ~kind))

let explicit (model_stm : mu_stm) (old_body : Ast.Compound_stm.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    old_body |> ensure_statements >>= block_list model_stm
    >>| Fir.Flow_block.explicit
    >>| A.construct Fir.Statement.flow)

let rec model : Ast.Stm.t -> unit Fir.Statement.t Or_error.t = function
  | Expr None ->
      Ok A.(construct Fir.(Statement.prim' @> Prim_statement.nop) ())
  | Expr (Some e) ->
      expr_stm (debracket e)
  | If {cond; t_branch; f_branch} ->
      model_if model cond t_branch f_branch
  | Compound xs ->
      explicit model xs
  | Continue ->
      Ok
        A.(
          construct
            Fir.(Statement.prim' @> Prim_statement.early_out)
            Continue)
  | Break ->
      Ok
        A.(construct Fir.(Statement.prim' @> Prim_statement.early_out) Break)
  | Return None ->
      Ok
        A.(
          construct Fir.(Statement.prim' @> Prim_statement.early_out) Return)
  | Return (Some _) as s ->
      Or_error.error_s
        [%message "Value returns not supported in FIR" ~got:(s : Ast.Stm.t)]
  | Atomic b ->
      lock model b Atomic
  | Synchronized b ->
      lock model b Synchronized
  | For {init; cond; update; body} ->
      for_loop model init cond update body
  | While (c, b) ->
      while_loop model c b While
  | Do_while (b, c) ->
      while_loop model c b Do_while
  | Label (Normal l, Expr None) ->
      (* This is a particularly weird subset of the labels, but I'm not sure
         how best to expand it. *)
      Ok A.(construct Fir.(Statement.prim' @> Prim_statement.label) l)
  | Goto l ->
      Ok A.(construct Fir.(Statement.prim' @> Prim_statement.goto) l)
  | (Label _ | Switch _) as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]
