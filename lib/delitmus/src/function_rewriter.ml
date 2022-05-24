(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module type S = sig
  val rewrite_all :
       unit Fir.Function.t Common.C_named.t list
    -> context:Context.t
    -> unit Fir.Function.t Common.C_named.t list Or_error.t
end

type 'a local_rw_fun = 'a -> tid:int -> context:Context.t -> 'a Or_error.t

type 'a rw_fun = 'a -> 'a Or_error.t

module Make (Basic : sig
  val rewrite_local_address : Fir.Address.t rw_fun

  val rewrite_local_lvalue : Fir.Lvalue.t rw_fun

  val rewrite_local_cid : Common.C_id.t local_rw_fun

  val rewrite_global_address : Fir.Address.t rw_fun

  val rewrite_global_lvalue : Fir.Lvalue.t rw_fun
end) =
struct
  module Rewriter_with_thread (Ctx : sig
    val thread : Thread.t

    val context : Context.t
  end) =
  struct
    let rewrite_lvalue_if_global : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
      Thread.when_global Ctx.thread ~over:Fir.Lvalue.variable_of
        ~f:Basic.rewrite_global_lvalue

    let rewrite_address_if_global : Fir.Address.t -> Fir.Address.t Or_error.t
        =
      Thread.when_global Ctx.thread ~over:Fir.Address.variable_of
        ~f:Basic.rewrite_global_address

    let rewrite_lvalue_if_local : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
      Thread.when_local Ctx.thread ~over:Fir.Lvalue.variable_of
        ~f:Basic.rewrite_local_lvalue

    let rewrite_address_if_local : Fir.Address.t -> Fir.Address.t Or_error.t
        =
      Thread.when_local Ctx.thread ~over:Fir.Address.variable_of
        ~f:Basic.rewrite_local_address

    let rewrite_id_if_local : Common.C_id.t -> Common.C_id.t Or_error.t =
      Thread.when_local Ctx.thread ~over:Accessor.id
        ~f:(Basic.rewrite_local_cid ~context:Ctx.context ~tid:Ctx.thread.tid)

    module C_stm_meta = Fir.Statement_traverse.With_meta (Unit)

    let rewrite_ids : unit Fir.Statement.t -> unit Fir.Statement.t Or_error.t
        =
      C_stm_meta.On_lvalues.With_errors.map_m
        ~f:
          (Utils.Accessor.On_error.map Fir.Lvalue.variable_of
             ~f:rewrite_id_if_local )

    (* When rewriting global lvalues (as part of a var-as-global run), we do
       it _after_ address rewriting, as the address rewriting will
       temporarily _increase_ the amount of indirection.

       When rewriting local lvalues (as part of a var-as-parameter run), we
       do it _before_ address rewriting, as the address rewriting will
       temporarily _decrease_ the amount of indirection. *)

    let rewrite_global_lvalues :
        unit Fir.Statement.t -> unit Fir.Statement.t Or_error.t =
      C_stm_meta.On_lvalues.With_errors.map_m ~f:rewrite_lvalue_if_global

    let rewrite_local_lvalues :
        unit Fir.Statement.t -> unit Fir.Statement.t Or_error.t =
      C_stm_meta.On_lvalues.With_errors.map_m ~f:rewrite_lvalue_if_local

    let rewrite_addresses :
        unit Fir.Statement.t -> unit Fir.Statement.t Or_error.t =
      C_stm_meta.On_addresses.With_errors.map_m
        ~f:
          Tx.Or_error.(
            rewrite_address_if_local >=> rewrite_address_if_global)

    let rewrite_statement :
        unit Fir.Statement.t -> unit Fir.Statement.t Or_error.t =
      Tx.Or_error.(
        rewrite_local_lvalues >=> rewrite_addresses
        >=> rewrite_global_lvalues >=> rewrite_ids)

    let rewrite_statements :
        unit Fir.Statement.t list -> unit Fir.Statement.t list Or_error.t =
      Tx.Or_error.combine_map ~f:rewrite_statement

    let filter_by_scope :
           (Common.Litmus_id.t, Var_map.Record.t) List.Assoc.t
        -> (Common.Litmus_id.t, Var_map.Record.t) List.Assoc.t =
      List.filter ~f:(fun (k, _) ->
          Common.Litmus_id.is_in_local_scope k ~from:Ctx.thread.tid )

    let expand_parameter ({c_id; c_type; _} : Var_map.Record.t) :
        (Common.C_id.t * Fir.Type.t) Or_error.t =
      Or_error.Let_syntax.(
        let%map pty = Fir.Type.ref c_type in
        (c_id, pty))

    let expand_parameters :
           (Common.Litmus_id.t, Var_map.Record.t) List.Assoc.t
        -> (Common.C_id.t, Fir.Type.t) List.Assoc.t Or_error.t =
      Tx.Or_error.combine_map ~f:(fun (_, r) -> expand_parameter r)

    let populate_parameters () :
        (Common.C_id.t, Fir.Type.t) List.Assoc.t Or_error.t =
      Ctx.context |> Context.var_map |> Var_map.param_mapped_vars
      |> filter_by_scope |> expand_parameters

    module F = Fir.Function.On (Or_error)

    let rewrite : unit Fir.Function.t -> unit Fir.Function.t Or_error.t =
      F.map_m
        ~parameters:(fun _ -> populate_parameters ())
        ~body_decls:(fun _ -> Ok [])
        ~body_stms:rewrite_statements
  end

  let rewrite (tid : int) (func : unit Fir.Function.t) ~(context : Context.t)
      : unit Fir.Function.t Or_error.t =
    let locals =
      func |> Fir.Function.body_decls |> List.map ~f:fst
      |> Set.of_list (module Common.C_id)
    in
    let module M = Rewriter_with_thread (struct
      let thread = {Thread.tid; locals}

      let context = context
    end) in
    M.rewrite func

  let lookup_function (name : Common.C_id.t) ~(context : Context.t) :
      Function_map.Record.t Or_error.t =
    let fn =
      (Context.aux context).@(Aux.function_map @> Accessor.Map.at name)
    in
    Result.of_option fn
      ~error:
        (Error.create_s
           [%message
             "Could not find function in function map."
               ~(name : Common.C_id.t)] )

  let rewrite_function_name (name : Common.C_id.t) ~(context : Context.t) :
      Common.C_id.t Or_error.t =
    Or_error.map
      (lookup_function name ~context)
      ~f:(Accessor.get Function_map.Record.c_id)

  let rewrite_named (tid : int) (fn : unit Fir.Function.t Common.C_named.t)
      ~(context : Context.t) :
      unit Fir.Function.t Common.C_named.t Or_error.t =
    Common.C_named.With_errors.bi_map_m fn
      ~left:(rewrite_function_name ~context)
      ~right:(rewrite tid ~context)

  let rewrite_all (fs : unit Fir.Function.t Common.C_named.t list)
      ~(context : Context.t) :
      unit Fir.Function.t Common.C_named.t list Or_error.t =
    fs |> List.mapi ~f:(rewrite_named ~context) |> Or_error.combine_errors
end

module Vars_as_globals = Make (struct
  let rewrite_global_address (addr : Fir.Address.t) :
      Fir.Address.t Or_error.t =
    let is_deref =
      Accessor.exists Fir.Address.lvalue_of addr ~f:Fir.Lvalue.is_deref
    in
    let addr' =
      if is_deref then addr
      else
        (* The added deref here will be removed in lvalue rewriting. *)
        Ref
          (Accessor.map Fir.Address.lvalue_of
             ~f:(Accessor.construct Fir.Lvalue.deref)
             addr )
    in
    Ok addr'

  let rewrite_global_lvalue : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
    Fir.Lvalue.un_deref

  let rewrite_local_address : Fir.Address.t -> Fir.Address.t Or_error.t =
    Or_error.return

  let rewrite_local_lvalue : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
    Or_error.return

  let rewrite_local_cid (cid : Common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Common.C_id.t Or_error.t =
    let vm = Context.var_map context in
    Var_map.lookup_and_require_global vm ~id:(Common.Litmus_id.local tid cid)
end)

module Vars_as_parameters = Make (struct
  let rewrite_global_address : Fir.Address.t -> Fir.Address.t Or_error.t =
    Or_error.return

  let rewrite_global_lvalue : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
    Or_error.return

  let rewrite_local_lvalue : Fir.Lvalue.t -> Fir.Lvalue.t Or_error.t =
    (* The added deref here will be removed in address rewriting if not
       needed. *)
    Fn.compose Or_error.return (Accessor.construct Fir.Lvalue.deref)

  let rewrite_local_address : Fir.Address.t -> Fir.Address.t Or_error.t =
    Fn.compose Or_error.return Fir.Address.normalise

  let rewrite_local_cid (cid : Common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Common.C_id.t Or_error.t =
    let vm = Context.var_map context in
    Var_map.lookup_and_require_param vm ~id:(Common.Litmus_id.local tid cid)
end)
