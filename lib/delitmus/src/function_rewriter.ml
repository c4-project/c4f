(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module C = Act_c_mini
module Tx = Travesty_base_exts

module type S = sig
  val rewrite_all :
       unit Act_c_mini.Function.t Act_c_mini.Named.t list
    -> context:Context.t
    -> unit Act_c_mini.Function.t Act_c_mini.Named.t list Or_error.t
end

type 'a local_rw_fun = 'a -> tid:int -> context:Context.t -> 'a Or_error.t

type 'a rw_fun = 'a -> 'a Or_error.t

module Make (Basic : sig
  val rewrite_local_address : C.Address.t rw_fun

  val rewrite_local_lvalue : C.Lvalue.t rw_fun

  val rewrite_local_cid : Act_common.C_id.t local_rw_fun

  val rewrite_global_address : C.Address.t rw_fun

  val rewrite_global_lvalue : C.Lvalue.t rw_fun
end) =
struct
  module Rewriter_with_thread (Ctx : sig
    module T : Thread.S

    val context : Context.t
  end) =
  struct
    let rewrite_lvalue_if_global : C.Lvalue.t -> C.Lvalue.t Or_error.t =
      Ctx.T.when_global ~over:C.Lvalue.variable_of
        ~f:Basic.rewrite_global_lvalue

    let rewrite_address_if_global : C.Address.t -> C.Address.t Or_error.t =
      Ctx.T.when_global ~over:C.Address.variable_of
        ~f:Basic.rewrite_global_address

    let rewrite_lvalue_if_local : C.Lvalue.t -> C.Lvalue.t Or_error.t =
      Ctx.T.when_local ~over:C.Lvalue.variable_of
        ~f:Basic.rewrite_local_lvalue

    let rewrite_address_if_local : C.Address.t -> C.Address.t Or_error.t =
      Ctx.T.when_local ~over:C.Address.variable_of
        ~f:Basic.rewrite_local_address

    let rewrite_id_if_local :
        Act_common.C_id.t -> Act_common.C_id.t Or_error.t =
      Ctx.T.when_local ~over:Fn.id
        ~f:(Basic.rewrite_local_cid ~context:Ctx.context ~tid:Ctx.T.tid)

    module C_stm_meta = C.Statement.With_meta (Unit)

    let rewrite_ids : unit C.Statement.t -> unit C.Statement.t Or_error.t =
      C_stm_meta.On_identifiers.With_errors.map_m ~f:rewrite_id_if_local

    (* When rewriting global lvalues (as part of a var-as-global run), we do
       it _after_ address rewriting, as the address rewriting will
       temporarily _increase_ the amount of indirection.

       When rewriting local lvalues (as part of a var-as-parameter run), we
       do it _before_ address rewriting, as the address rewriting will
       temporarily _decrease_ the amount of indirection. *)

    let rewrite_global_lvalues :
        unit C.Statement.t -> unit C.Statement.t Or_error.t =
      C_stm_meta.On_lvalues.With_errors.map_m ~f:rewrite_lvalue_if_global

    let rewrite_local_lvalues :
        unit C.Statement.t -> unit C.Statement.t Or_error.t =
      C_stm_meta.On_lvalues.With_errors.map_m ~f:rewrite_lvalue_if_local

    let rewrite_addresses :
        unit C.Statement.t -> unit C.Statement.t Or_error.t =
      C_stm_meta.On_addresses.With_errors.map_m
        ~f:
          Tx.Or_error.(
            rewrite_address_if_local >=> rewrite_address_if_global)

    let rewrite_statement :
        unit C.Statement.t -> unit C.Statement.t Or_error.t =
      Tx.Or_error.(
        rewrite_local_lvalues >=> rewrite_addresses
        >=> rewrite_global_lvalues >=> rewrite_ids)

    let rewrite_statements :
        unit C.Statement.t list -> unit C.Statement.t list Or_error.t =
      Tx.Or_error.combine_map ~f:rewrite_statement

    let expand_parameter (id : Act_common.Litmus_id.t)
        (record : Var_map.Record.t) :
        (Act_common.C_id.t * C.Type.t) Or_error.t =
      let ty = Var_map.Record.c_type record in
      Or_error.Let_syntax.(
        let%map pty = C.Type.ref ty in
        (Act_common.Litmus_id.variable_name id, pty))

    let populate_parameters () :
        (Act_common.C_id.t, C.Type.t) List.Assoc.t Or_error.t =
      (* We assume that all variables that aren't mapped to global variables,
         and relevant to this thread, are supposed to be passed in as
         parameters. *)
      let var_map = Context.var_map Ctx.context in
      let all_unmapped = Var_map.globally_unmapped_vars var_map in
      let relevant_unmapped =
        List.filter
          ~f:(fun (k, _) ->
            Act_common.Litmus_id.is_in_scope k ~from:Ctx.T.tid)
          all_unmapped
      in
      Tx.Or_error.combine_map
        ~f:(fun (i, r) -> expand_parameter i r)
        relevant_unmapped

    module F = C.Function.On_monad (Or_error)

    let rewrite : unit C.Function.t -> unit C.Function.t Or_error.t =
      F.map_m
        ~parameters:(fun _ -> populate_parameters ())
        ~body_decls:(fun _ -> Or_error.return [])
        ~body_stms:rewrite_statements
  end

  let rewrite (tid : int) (func : unit C.Function.t) ~(context : Context.t) :
      unit C.Function.t Or_error.t =
    let module T = Thread.Make (struct
      let tid = tid

      let locals =
        func |> C.Function.body_decls |> List.map ~f:fst
        |> Set.of_list (module Act_common.C_id)
    end) in
    let module M = Rewriter_with_thread (struct
      module T = T

      let context = context
    end) in
    M.rewrite func

  let lookup_function (name : Act_common.C_id.t) ~(context : Context.t) :
      Function_map.Record.t Or_error.t =
    let fmap = context |> Context.aux |> Aux.function_map in
    Map.find fmap name
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "Could not find function in function map."
                  ~name:(name : Act_common.C_id.t)])

  let rewrite_function_name (name : Act_common.C_id.t) ~(context : Context.t)
      : Act_common.C_id.t Or_error.t =
    Or_error.map (lookup_function name ~context) ~f:Function_map.Record.c_id

  let rewrite_named (tid : int) (fn : unit C.Function.t C.Named.t)
      ~(context : Context.t) : unit C.Function.t C.Named.t Or_error.t =
    C.Named.With_errors.bi_map_m fn
      ~left:(rewrite_function_name ~context)
      ~right:(rewrite tid ~context)

  let rewrite_all (fs : unit C.Function.t C.Named.t list)
      ~(context : Context.t) : unit C.Function.t C.Named.t list Or_error.t =
    fs |> List.mapi ~f:(rewrite_named ~context) |> Or_error.combine_errors
end

module Vars_as_globals = Make (struct
  let rewrite_global_address (addr : C.Address.t) : C.Address.t Or_error.t =
    let is_deref = C.Address.On_lvalues.exists addr ~f:C.Lvalue.is_deref in
    let addr' =
      if is_deref then addr
      else
        (* The added deref here will be removed in lvalue rewriting. *)
        C.Address.ref (C.Address.On_lvalues.map ~f:C.Lvalue.deref addr)
    in
    Or_error.return addr'

  let rewrite_global_lvalue : C.Lvalue.t -> C.Lvalue.t Or_error.t =
    C.Lvalue.un_deref

  let rewrite_local_address : C.Address.t -> C.Address.t Or_error.t =
    Or_error.return

  let rewrite_local_lvalue : C.Lvalue.t -> C.Lvalue.t Or_error.t =
    Or_error.return

  let rewrite_local_cid (cid : Act_common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Act_common.C_id.t Or_error.t =
    let vm = Context.var_map context in
    Var_map.lookup_and_require_global vm
      ~id:(Act_common.Litmus_id.local tid cid)
end)

module Vars_as_parameters = Make (struct
  let rewrite_global_address : C.Address.t -> C.Address.t Or_error.t =
    Or_error.return

  let rewrite_global_lvalue : C.Lvalue.t -> C.Lvalue.t Or_error.t =
    Or_error.return

  let rewrite_local_lvalue : C.Lvalue.t -> C.Lvalue.t Or_error.t =
    (* The added deref here will be removed in address rewriting if not
       needed. *)
    Fn.compose Or_error.return C.Lvalue.deref

  let rewrite_local_address : C.Address.t -> C.Address.t Or_error.t =
    Fn.compose Or_error.return C.Address.normalise

  let rewrite_local_cid (cid : Act_common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Act_common.C_id.t Or_error.t =
    ignore (tid : int) ;
    ignore (context : Context.t) ;
    Or_error.return cid
end)
