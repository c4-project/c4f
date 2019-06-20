(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module C = Act_c
module Tx = Travesty_base_exts

module type S = [%import: (module Function_rewriter.S)]

type 'a local_rw_fun = 'a -> tid:int -> context:Context.t -> 'a Or_error.t

type 'a rw_fun = 'a -> 'a Or_error.t

module Make (Basic : sig
  val rewrite_local_address : Act_c.Mini.Address.t rw_fun

  val rewrite_local_lvalue : Act_c.Mini.Lvalue.t rw_fun

  val rewrite_local_cid : Act_common.C_id.t local_rw_fun

  val rewrite_global_address : Act_c.Mini.Address.t rw_fun

  val rewrite_global_lvalue : Act_c.Mini.Lvalue.t rw_fun
end) =
struct
  module Rewriter_with_thread (Ctx : sig
    module T : Thread.S

    val context : Context.t
  end) =
  struct
    let rewrite_lvalue_if_global :
        Act_c.Mini.Lvalue.t -> Act_c.Mini.Lvalue.t Or_error.t =
      Ctx.T.when_global ~over:Act_c.Mini.Lvalue.variable_of
        ~f:Basic.rewrite_global_lvalue

    let rewrite_address_if_global :
        Act_c.Mini.Address.t -> Act_c.Mini.Address.t Or_error.t =
      Ctx.T.when_global ~over:Act_c.Mini.Address.variable_of
        ~f:Basic.rewrite_global_address

    let rewrite_lvalue_if_local :
        Act_c.Mini.Lvalue.t -> Act_c.Mini.Lvalue.t Or_error.t =
      Ctx.T.when_local ~over:Act_c.Mini.Lvalue.variable_of
        ~f:Basic.rewrite_local_lvalue

    let rewrite_address_if_local :
        Act_c.Mini.Address.t -> Act_c.Mini.Address.t Or_error.t =
      Ctx.T.when_local ~over:Act_c.Mini.Address.variable_of
        ~f:Basic.rewrite_local_address

    let rewrite_id_if_local :
        Act_common.C_id.t -> Act_common.C_id.t Or_error.t =
      Ctx.T.when_local ~over:Fn.id
        ~f:(Basic.rewrite_local_cid ~context:Ctx.context ~tid:Ctx.T.tid)

    let rewrite_ids :
        Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t Or_error.t =
      Act_c.Mini.Statement.On_identifiers.With_errors.map_m
        ~f:rewrite_id_if_local

    (* When rewriting global lvalues (as part of a var-as-global run), we do
       it _after_ address rewriting, as the address rewriting will
       temporarily _increase_ the amount of indirection.

       When rewriting local lvalues (as part of a var-as-parameter run), we
       do it _before_ address rewriting, as the address rewriting will
       temporarily _decrease_ the amount of indirection. *)

    let rewrite_global_lvalues :
        Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t Or_error.t =
      Act_c.Mini.Statement.On_lvalues.With_errors.map_m
        ~f:rewrite_lvalue_if_global

    let rewrite_local_lvalues :
        Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t Or_error.t =
      Act_c.Mini.Statement.On_lvalues.With_errors.map_m
        ~f:rewrite_lvalue_if_local

    let rewrite_addresses :
        Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t Or_error.t =
      Act_c.Mini.Statement.On_addresses.With_errors.map_m
        ~f:
          Tx.Or_error.(
            rewrite_address_if_local >=> rewrite_address_if_global)

    let rewrite_statement :
        C.Mini.Statement.t -> C.Mini.Statement.t Or_error.t =
      Tx.Or_error.(
        rewrite_local_lvalues >=> rewrite_addresses
        >=> rewrite_global_lvalues >=> rewrite_ids)

    let rewrite_statements :
        C.Mini.Statement.t list -> C.Mini.Statement.t list Or_error.t =
      Tx.Or_error.combine_map ~f:rewrite_statement

    let expand_parameter (id : Act_common.Litmus_id.t) :
        (Act_common.C_id.t * Act_c.Mini.Type.t) Or_error.t =
      Or_error.Let_syntax.(
        let%bind ty = Context.lookup_type Ctx.context ~id in
        let%map pty = Act_c.Mini.Type.ref ty in
        (Act_common.Litmus_id.variable_name id, pty))

    let populate_parameters () :
        (Act_common.C_id.t, Act_c.Mini.Type.t) List.Assoc.t Or_error.t =
      (* We assume that all variables that aren't mapped to global
         variables, and relevant to this thread, are supposed to be passed
         in as parameters. *)
      let all_unmapped = Context.unmapped_litmus_ids Ctx.context in
      let relevant_unmapped =
        Set.filter
          ~f:(Act_common.Litmus_id.is_in_scope ~from:Ctx.T.tid)
          all_unmapped
      in
      relevant_unmapped |> Set.to_list
      |> Tx.Or_error.combine_map ~f:expand_parameter

    module F = C.Mini.Function.On_monad (Or_error)

    let rewrite (func : C.Mini.Function.t) =
      F.map_m func
        ~parameters:(fun _ -> populate_parameters ())
        ~body_decls:(fun _ -> Or_error.return [])
        ~body_stms:rewrite_statements
  end

  let rewrite (tid : int) (func : C.Mini.Function.t) ~(context : Context.t)
      : C.Mini.Function.t Or_error.t =
    let module T = Thread.Make (struct
      let tid = tid

      let locals =
        func |> C.Mini.Function.body_decls |> List.map ~f:fst
        |> C.Mini.Identifier.Set.of_list
    end) in
    let module M = Rewriter_with_thread (struct
      module T = T

      let context = context
    end) in
    M.rewrite func

  let rewrite_all (fs : C.Mini.Function.t C.Mini_intf.id_assoc)
      ~(context : Context.t) :
      C.Mini.Function.t C.Mini_intf.id_assoc Or_error.t =
    Tx.List.With_errors.mapi_m fs ~f:(fun tid ->
        Tx.Tuple2.With_errors.map_right_m ~f:(rewrite tid ~context) )
end

module Vars_as_globals = Make (struct
  let rewrite_global_address (addr : Act_c.Mini.Address.t) :
      Act_c.Mini.Address.t Or_error.t =
    let is_deref =
      C.Mini.Address.On_lvalues.exists addr ~f:C.Mini.Lvalue.is_deref
    in
    let addr' =
      if is_deref then addr
      else
        (* The added deref here will be removed in lvalue rewriting. *)
        C.Mini.Address.ref
          (C.Mini.Address.On_lvalues.map ~f:C.Mini.Lvalue.deref addr)
    in
    Or_error.return addr'

  let rewrite_global_lvalue :
      Act_c.Mini.Lvalue.t -> Act_c.Mini.Lvalue.t Or_error.t =
    Act_c.Mini.Lvalue.un_deref

  let rewrite_local_address (addr : Act_c.Mini.Address.t) :
      Act_c.Mini.Address.t Or_error.t =
    Or_error.return addr

  let rewrite_local_lvalue (lv : Act_c.Mini.Lvalue.t) :
      Act_c.Mini.Lvalue.t Or_error.t =
    Or_error.return lv

  let rewrite_local_cid (cid : Act_common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Act_common.C_id.t Or_error.t =
    Context.lookup_and_require_global context
      ~id:(Act_common.Litmus_id.local tid cid)
end)

module Vars_as_parameters = Make (struct
  let rewrite_global_address :
      Act_c.Mini.Address.t -> Act_c.Mini.Address.t Or_error.t =
    Or_error.return

  let rewrite_global_lvalue :
      Act_c.Mini.Lvalue.t -> Act_c.Mini.Lvalue.t Or_error.t =
    Or_error.return

  let rewrite_local_lvalue :
      Act_c.Mini.Lvalue.t -> Act_c.Mini.Lvalue.t Or_error.t =
    (* The added deref here will be removed in address rewriting if not
       needed. *)
    Fn.compose Or_error.return C.Mini.Lvalue.deref

  let rewrite_local_address :
      Act_c.Mini.Address.t -> Act_c.Mini.Address.t Or_error.t =
    Fn.compose Or_error.return C.Mini.Address.normalise

  let rewrite_local_cid (cid : Act_common.C_id.t) ~(tid : int)
      ~(context : Context.t) : Act_common.C_id.t Or_error.t =
    ignore (tid : int) ;
    ignore (context : Context.t) ;
    Or_error.return cid
end)
