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

module Make (Basic : sig
  (* TODO(@MattWindsor91): add the differences between drivers here. *)
  val rewrite_local_cid : Act_common.C_id.t -> tid:int -> context:Context.t -> Act_common.C_id.t Or_error.t
end) =
struct
  module Rewriter_with_thread (Ctx : sig
      module T : Thread.S
      val context : Context.t
    end) = struct
    (** [address_globals stm] converts each address in [stm] over a global
        variable [v] to [&*v], ready for {{!ref_globals} ref_globals} to
        reduce to [&v]. *)
    let address_globals : C.Mini.Statement.t -> C.Mini.Statement.t Or_error.t =
      C.Mini.Statement.On_addresses.With_errors.map_m
        ~f:
          (Ctx.T.when_global ~over:C.Mini.Address.variable_of ~f:(fun addr ->
               (* The added deref here will be removed in [ref_globals]. *)
               Or_error.return (
               C.Mini.Address.ref
                 (C.Mini.Address.On_lvalues.map ~f:C.Mini.Lvalue.deref addr))
           ))

    (** [ref_globals stm] turns all dereferences of global variables in
        [stm] into direct accesses to the same variables. *)
    let ref_globals : C.Mini.Statement.t -> C.Mini.Statement.t Or_error.t =
      C.Mini.Statement.On_lvalues.With_errors.map_m
        ~f:
          C.Mini.Lvalue.(
            Ctx.T.when_global ~over:variable_of
              ~f:(Fn.compose Or_error.return (Fn.compose variable variable_of)))

    let rewrite_locals_in_statement :
      Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t Or_error.t =
      Act_c.Mini.Statement.On_identifiers.With_errors.map_m
        ~f:(Ctx.T.when_local ~over:Fn.id ~f:(Basic.rewrite_local_cid ~context:Ctx.context ~tid:Ctx.T.tid))

    let rewrite_statement (stm : C.Mini.Statement.t) : C.Mini.Statement.t Or_error.t =
      Or_error.(stm |> address_globals >>= ref_globals
                                           >>= rewrite_locals_in_statement)

    let rewrite_statements :
        C.Mini.Statement.t list -> C.Mini.Statement.t list Or_error.t =
      Tx.Or_error.combine_map ~f:rewrite_statement

    let expand_parameter (id : Act_common.Litmus_id.t)
        : (Act_common.C_id.t * Act_c.Mini.Type.t) Or_error.t =
      Or_error.Let_syntax.(
        let%bind ty = Context.lookup_type Ctx.context ~id in
        let%map pty = Act_c.Mini.Type.ref ty in
        (Act_common.Litmus_id.variable_name id, pty))

    let populate_parameters () :
      (Act_common.C_id.t, Act_c.Mini.Type.t) List.Assoc.t Or_error.t =
      (* We assume that all variables that aren't mapped to global variables,
         and relevant to this thread, are supposed to be passed in as
         parameters. *)
      let all_unmapped = Context.unmapped_litmus_ids (Ctx.context) in
      let relevant_unmapped =
        Set.filter ~f:(Act_common.Litmus_id.is_in_scope ~from:Ctx.T.tid)
          all_unmapped
      in
      relevant_unmapped
      |> Set.to_list
      |> Tx.Or_error.combine_map ~f:expand_parameter

    module F = C.Mini.Function.On_monad(Or_error)

    let rewrite (func : C.Mini.Function.t) =
      F.map_m func
        ~parameters:(fun _ -> populate_parameters ())
        ~body_decls:(fun _ -> Or_error.return []) ~body_stms:rewrite_statements
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
    let rewrite_local_cid (cid: Act_common.C_id.t) ~(tid: int) ~(context: Context.t) : Act_common.C_id.t Or_error.t =
    Context.lookup_and_require_global context
      ~id:(Act_common.Litmus_id.local tid cid)
  end)

module Vars_as_parameters = Make (struct
    let rewrite_local_cid (cid: Act_common.C_id.t) ~(tid: int) ~(context: Context.t) :
      Act_common.C_id.t Or_error.t =
  ignore (tid : int);
  ignore (context : Context.t);
  Or_error.return cid
  end)
