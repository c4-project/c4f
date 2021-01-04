(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): eventually, it would be nice for this to use the
   standard FIR traversals. This would probably require a nice monoid for
   (expression-level) statsets, to allow summing up of expression results. *)

module Statset = struct
  type t =
    { threads: int
    ; returns: int
    ; literal_bools: int
    ; expr_atomics: int Map.M(Atomic_class).t
    ; expr_mos: int Map.M(Mem_order).t
    ; stm_atomics: int Map.M(Atomic_class).t
    ; stm_mos: int Map.M(Mem_order).t }
  [@@deriving fields]

  let init_enum (type t cw)
      (module M : Act_utils.Enum_types.Extension
        with type t = t
         and type comparator_witness = cw) : (t, int, cw) Map.t =
    M.all_list ()
    |> List.map ~f:(fun x -> (x, 0))
    |> Map.of_alist_exn (module M)

  let init (threads : int) : t =
    { threads
    ; returns= 0
    ; literal_bools= 0
    ; expr_atomics= init_enum (module Atomic_class)
    ; expr_mos= init_enum (module Mem_order)
    ; stm_atomics= init_enum (module Atomic_class)
    ; stm_mos= init_enum (module Mem_order) }

  let pp_map (pp_k : 'k Fmt.t) (pp_v : 'v Fmt.t) : ('k, 'v, 'cw) Map.t Fmt.t
      =
    Fmt.(
      vbox
        (using Map.to_alist (list ~sep:sp (hbox (pair ~sep:sp pp_k pp_v)))))

  let pp_atomic_name (ty : string) : Atomic_class.t Fmt.t =
    Fmt.(
      concat ~sep:(any ".")
        [any "atomics"; const string ty; using Atomic_class.to_string string])

  let pp_atomics (ty : string) : int Map.M(Atomic_class).t Fmt.t =
    pp_map (pp_atomic_name ty) Fmt.int

  let pp_mo_name (ty : string) : Mem_order.t Fmt.t =
    Fmt.(
      concat ~sep:(any ".")
        [any "mem-orders"; const string ty; using Mem_order.to_string string])

  let pp_mos (ty : string) : int Map.M(Mem_order).t Fmt.t =
    pp_map (pp_mo_name ty) Fmt.int

  let pp : t Fmt.t =
    Fmt.(
      record
        [ field ~sep:sp "threads" threads int
        ; field ~sep:sp "returns" returns int
        ; field ~sep:sp "literals.bool" literal_bools int
        ; using expr_atomics (pp_atomics "expression")
        ; using expr_mos (pp_mos "expression")
        ; using stm_atomics (pp_atomics "statement")
        ; using stm_mos (pp_mos "statement") ])
end

module Monad = Travesty.State.Make (Statset)
module MList = Travesty_base_exts.List.On_monad (Monad)
module MPExpr = Prim_statement.On_expressions.On_monad (Monad)

let nowt (type a) (_ : a) : unit Monad.t = Monad.return ()

let up_counter (ctr : _ Field.t) (k : int) : unit Monad.t =
  Monad.modify (Field.map ctr ~f:(fun n -> n + k))

let up_mapping (type k cw) (map : (k, int, cw) Map.t) (key : k) :
    (k, int, cw) Map.t =
  Map.update map key ~f:(fun i -> Option.value i ~default:0 + 1)

module Stm = Statement_traverse.With_meta (Unit)
module K =
  Travesty.Traversable.Chain0
    (Stm.On_expressions)
    (Expression_traverse.On_constants)

let probe_fn_decl (decl : Initialiser.t) : unit Monad.t =
  let literal_bools =
    Accessor.count Initialiser.value decl ~f:Constant.is_bool
  in
  up_counter Statset.Fields.literal_bools literal_bools

let probe_fn_decls (decls : Initialiser.t Act_common.C_named.Alist.t) :
    unit Monad.t =
  MList.iter_m decls ~f:(fun (_, d) -> probe_fn_decl d)

let add_classification (m : int Map.M(Atomic_class).t) :
    Atomic_class.t option -> int Map.M(Atomic_class).t =
  Option.value_map ~default:m ~f:(up_mapping m)

let classify_atomic_stm (atom : Atomic_statement.t)
    (m : int Map.M(Atomic_class).t) : int Map.M(Atomic_class).t =
  atom |> Atomic_class.classify_stm |> add_classification m

module MEME = Atomic_expression.On_expressions.On_monad (Monad)
module MEMO = Expression_traverse.Atomic.On_mem_orders.On_monad (Monad)
module MSMO = Atomic_statement.On_mem_orders.On_monad (Monad)

let probe_atomic_stm (atom : Atomic_statement.t) : unit Monad.t =
  Monad.(
    Let_syntax.(
      let%bind () =
        modify
          (Field.map Statset.Fields.stm_atomics
             ~f:(classify_atomic_stm atom))
      in
      MSMO.iter_m atom ~f:(fun mo ->
          modify
            (Field.map Statset.Fields.stm_mos ~f:(fun map ->
                 up_mapping map mo)))))

let classify_atomic_expr (atom : _ Atomic_expression.t)
    (m : int Map.M(Atomic_class).t) : int Map.M(Atomic_class).t =
  atom |> Atomic_class.classify_expr |> add_classification m

let probe_atomic_expr (atom : _ Atomic_expression.t) : unit Monad.t =
  Monad.(
    Let_syntax.(
      (* TODO(@MattWindsor91): part of this is supposed to sequence through
         any inner state calls, but the replacement of them with dummy
         expressions is kind-of awful. *)
      let%bind atom =
        MEME.map_m atom ~f:(fun expr ->
            Monad.(expr >>| fun () -> Expression.falsehood))
      in
      let%bind () =
        modify
          (Field.map Statset.Fields.expr_atomics
             ~f:(classify_atomic_expr atom))
      in
      MEMO.iter_m atom ~f:(fun mo ->
          modify
            (Field.map Statset.Fields.expr_mos ~f:(fun map ->
                 up_mapping map mo)))))

let probe_constant (k : Constant.t) : unit Monad.t =
  Monad.when_m (Constant.is_bool k) ~f:(fun () ->
      up_counter Statset.Fields.literal_bools 1)

module AccM = Accessor.Of_monad (struct
  include Monad

  let apply = `Define_using_bind
end)

let probe_expr : Expression.t -> unit Monad.t =
  Expression.reduce ~constant:probe_constant ~address:nowt
    ~atomic:probe_atomic_expr
    ~bop:(fun _ l r ->
      Monad.Let_syntax.(
        let%bind () = l in
        r))
    ~uop:(fun _ u -> u)
    ~ternary:(AccM.iter Expr_ternary.exprs ~f:Fn.id)

let probe_early_out : Early_out.t -> unit Monad.t = function
  | Return ->
      up_counter Statset.Fields.returns 1
  | Break | Continue ->
      Monad.return ()

let probe_prim (s : Prim_statement.t) : unit Monad.t =
  Monad.Let_syntax.(
    let%bind () = MPExpr.iter_m s ~f:probe_expr in
    Prim_statement.value_map s ~assign:nowt ~atomic:probe_atomic_stm
      ~early_out:probe_early_out ~label:nowt ~goto:nowt ~nop:nowt
      ~procedure_call:nowt)

module AM = Accessor.Of_monad (struct
  include Monad

  let apply = `Define_using_bind
end)

let seq_block : (unit, unit Monad.t) Block.t -> unit Monad.t =
  AM.all_unit Block.each_statement

let probe_if (i : (unit, unit Monad.t) If.t) : unit Monad.t =
  Monad.all_unit
    [seq_block i.t_branch; seq_block i.f_branch; probe_expr i.cond]

module HdrM = Flow_block.Header.On_expressions.On_monad (Monad)

let probe_flow ({body; header} : (unit, unit Monad.t) Flow_block.t) :
    unit Monad.t =
  Monad.Let_syntax.(
    let%bind () = seq_block body in
    HdrM.iter_m ~f:probe_expr header)

let probe_fn_stm : unit Statement.t -> unit Monad.t =
  Statement.reduce
    ~prim:(fun {value; _} -> probe_prim value)
    ~if_stm:probe_if ~flow:probe_flow

let probe_fn_stms : unit Statement.t list -> unit Monad.t =
  MList.iter_m ~f:probe_fn_stm

let probe_fn (fn : unit Function.t) : unit Monad.t =
  Monad.Let_syntax.(
    let%bind () = probe_fn_decls (Function.body_decls fn) in
    probe_fn_stms (Function.body_stms fn))

let probe_named_fn (fn : unit Function.t Act_common.C_named.t) : unit Monad.t
    =
  probe_fn (Accessor.get Act_common.C_named.value fn)

let probe_threads : unit Function.t Act_common.C_named.t list -> unit Monad.t
    =
  MList.iter_m ~f:probe_named_fn

let scrape_expr (e : Expression.t) : Statset.t =
  fst (Monad.run' (probe_expr e) (Statset.init 0))

let scrape (t : Litmus.Test.t) : Statset.t =
  let ts = Litmus.Test.threads t in
  let threads = List.length ts in
  let state = Statset.init threads in
  fst (Monad.run' (probe_threads ts) state)
