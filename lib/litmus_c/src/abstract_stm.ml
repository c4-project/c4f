(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let ensure_statements :
    Ast.Compound_stm.Elt.t list -> Ast.Stm.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Stm f -> Ok f
    | d ->
        Or_error.error_s
          [%message "Expected a statement" ~got:(d : Ast.Compound_stm.Elt.t)] )

let expr = Abstract_expr.model

let model_atomic_cmpxchg_stm (args : Ast.Expr.t list)
    ~(strength : Fir.Atomic_cmpxchg.Strength.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_cmpxchg ~strength ~expr
    >>| Accessor.construct
          Fir.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.cmpxchg ) )

let model_atomic_fence_stm (args : Ast.Expr.t list)
    ~(mode : Fir.Atomic_fence.Mode.t) : unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fence ~mode
    >>| Accessor.construct
          Fir.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.fence ) )

let model_atomic_fetch_stm (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fetch ~expr ~op
    >>| Accessor.construct
          Fir.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.fetch ) )

let model_atomic_store_stm (args : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_store ~expr
    >>| Accessor.construct
          Fir.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.store ) )

let expr_stm_call_table :
    (Ast.Expr.t list -> unit Fir.Statement.t Or_error.t) Map.M(Common.C_id).t
    Lazy.t =
  lazy
    Abstract_atomic.(
      Map.of_alist_exn
        (module Common.C_id)
        ( cmpxchg_call_alist model_atomic_cmpxchg_stm
        @ fetch_call_alist model_atomic_fetch_stm
        @ fence_call_alist model_atomic_fence_stm
        @ [(Common.C_id.of_string store_name, model_atomic_store_stm)] ) )

let arbitrary_procedure_call (function_id : Common.C_id.t)
    (raw_arguments : Ast.Expr.t list) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map arguments = Tx.Or_error.combine_map ~f:expr raw_arguments in
    let call = Fir.Call.make ~arguments ~function_id () in
    Accessor.construct
      Fir.(Statement.prim' @> Prim_statement.procedure_call)
      call )

let procedure_call (func : Ast.Expr.t) (arguments : Ast.Expr.t list) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind function_id = Abstract_prim.expr_to_identifier func in
    match Map.find (Lazy.force expr_stm_call_table) function_id with
    | Some call_handler -> call_handler arguments
    | None -> arbitrary_procedure_call function_id arguments )

let lift_assign : Fir.Assign.t -> unit Fir.Statement.t =
  Accessor.construct Fir.(Statement.prim' @> Prim_statement.assign)

let crement (dir : [`Inc | `Dec]) (l : Ast.Expr.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map dst = Abstract_prim.expr_to_lvalue l in
    let src : Fir.Assign.Source.t =
      match dir with `Inc -> Inc | `Dec -> Dec
    in
    lift_assign (Fir.Assign.make ~dst ~src) )

let expr_stm : Ast.Expr.t -> unit Fir.Statement.t Or_error.t = function
  | Binary (l, `Assign, r) ->
      Or_error.Let_syntax.(
        let%map lvalue = Abstract_prim.expr_to_lvalue l
        and rvalue = expr r in
        lift_assign Fir.Assign.(lvalue @= rvalue) )
  | Call {func; arguments} -> procedure_call func arguments
  (* ++x and x++ SHOULD be semantically equivalent in statement position. *)
  | Postfix (l, `Inc) | Prefix (`Inc, l) -> crement `Inc l
  | Postfix (l, `Dec) | Prefix (`Dec, l) -> crement `Dec l
  | ( Brackets _ (* should've been debracketed already *) | Constant _
    | Prefix _ | Binary _ | Ternary _ | Cast _ | Subscript _ | Field _
    | Sizeof_type _ | String _ | Identifier _ ) as e ->
      Or_error.error_s
        [%message "Unsupported expression statement" ~got:(e : Ast.Expr.t)]

let possible_compound_to_list : Ast.Stm.t -> Ast.Stm.t list Or_error.t =
  function
  | Ast.Stm.Compound elems ->
      (* We don't support inner declarations at the moment. *)
      ensure_statements elems
  | stm -> Ok [stm]

(** Type of recursive statement converters. *)
type mu_stm = Ast.Stm.t -> unit Fir.Statement.t Or_error.t

let block_list (model_stm : mu_stm) (old_block : Ast.Stm.t list) :
    (unit, unit Fir.Statement.t) Fir.Block.t Or_error.t =
  Or_error.(
    Tx.Or_error.combine_map ~f:model_stm old_block
    >>| Fir.Block.of_statement_list )

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
    Accessor.construct Fir.Statement.if_stm ifs )

let rec debracket (x : Ast.Expr.t) : Ast.Expr.t =
  (* We do this inline in the abstraction for ordinary expressions, so this
     purely exists for statement/for-loop component expressions. *)
  match x with Brackets x -> debracket x | y -> y

let require_assign (e : Ast.Expr.t) : Fir.Assign.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind es = expr_stm e in
    let ao = es.@?(Fir.Statement.prim' @> Fir.Prim_statement.assign) in
    Result.of_option ao
      ~error:
        (Error.create_s
           [%message "Expected assignment here" ~e:(e : Ast.Expr.t)] ) )

let opt_map (x : 'a option) ~(f : 'a -> 'b Or_error.t) : 'b option Or_error.t
    =
  Utils.Accessor.On_error.map Accessor.Option.some ~f x

let for_loop (model_stm : mu_stm) (old_init_opt : Ast.Expr.t option)
    (old_cond_opt : Ast.Expr.t option) (old_update_opt : Ast.Expr.t option)
    (body : Ast.Stm.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind init = opt_map old_init_opt ~f:require_assign in
    let%bind cmp = opt_map old_cond_opt ~f:expr in
    let%bind update = opt_map old_update_opt ~f:require_assign in
    let%map body = block model_stm body in
    let control = Fir.Flow_block.For.make ?init ?cmp ?update () in
    Accessor.construct Fir.Statement.flow
      (Fir.Flow_block.for_loop body ~control) )

let while_loop (model_stm : mu_stm) (old_cond : Ast.Expr.t)
    (old_body : Ast.Stm.t) (kind : Fir.Flow_block.While.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    let%map cond = expr old_cond and body = block model_stm old_body in
    Accessor.construct Fir.Statement.flow
      (Fir.Flow_block.while_loop ~cond ~body ~kind) )

let lock (model_stm : mu_stm) (old_body : Ast.Compound_stm.t)
    (kind : Fir.Flow_block.Lock.t) : unit Fir.Statement.t Or_error.t =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): we should really support declarations here. *)
    let%bind ast_stms = ensure_statements old_body in
    let%map body = block_list model_stm ast_stms in
    Accessor.construct Fir.Statement.flow
      (Fir.Flow_block.lock_block body ~kind) )

let explicit (model_stm : mu_stm) (old_body : Ast.Compound_stm.t) :
    unit Fir.Statement.t Or_error.t =
  Or_error.(
    old_body |> ensure_statements >>= block_list model_stm
    >>| Fir.Flow_block.explicit
    >>| Accessor.construct Fir.Statement.flow )

let rec model : Ast.Stm.t -> unit Fir.Statement.t Or_error.t = function
  | Expr None ->
      Ok Accessor.(construct Fir.(Statement.prim' @> Prim_statement.nop) ())
  | Expr (Some e) -> expr_stm (debracket e)
  | If {cond; t_branch; f_branch} -> model_if model cond t_branch f_branch
  | Compound xs -> explicit model xs
  | Continue ->
      Ok
        Accessor.(
          construct
            Fir.(Statement.prim' @> Prim_statement.early_out)
            Continue )
  | Break ->
      Ok
        Accessor.(
          construct Fir.(Statement.prim' @> Prim_statement.early_out) Break )
  | Return None ->
      Ok
        Accessor.(
          construct Fir.(Statement.prim' @> Prim_statement.early_out) Return )
  | Return (Some _) as s ->
      Or_error.error_s
        [%message "Value returns not supported in FIR" ~got:(s : Ast.Stm.t)]
  | Atomic b -> lock model b Atomic
  | Synchronized b -> lock model b Synchronized
  | For {init; cond; update; body} -> for_loop model init cond update body
  | While (c, b) -> while_loop model c b While
  | Do_while (b, c) -> while_loop model c b Do_while
  | Label (Normal l, Expr None) ->
      (* This is a particularly weird subset of the labels, but I'm not sure
         how best to expand it. *)
      Ok Accessor.(construct Fir.(Statement.prim' @> Prim_statement.label) l)
  | Goto l ->
      Ok Accessor.(construct Fir.(Statement.prim' @> Prim_statement.goto) l)
  | (Label _ | Switch _) as s ->
      Or_error.error_s
        [%message "Unsupported statement" ~got:(s : Ast.Stm.t)]
