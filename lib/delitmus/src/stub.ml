(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let try_parse_program_id (id : Common.C_id.t) : int Or_error.t =
  let strid = Common.C_id.to_string id in
  Or_error.(
    tag ~tag:"Thread function does not have a well-formed name"
      (try_with (fun () -> Caml.Scanf.sscanf strid "P%d" Fn.id)))

let to_param_opt (lit_id : Common.Litmus_id.t) (rc : Var_map.Record.t) :
    (int * (Common.Litmus_id.t * Fir.Type.t)) option =
  match rc.mapped_to with
  | Param k -> Some (k, (lit_id, rc.c_type))
  | Global -> None

let to_sorted_params_opt
    (alist : (Common.Litmus_id.t, Var_map.Record.t) List.Assoc.t) :
    (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t =
  alist
  |> List.filter_map ~f:(fun (l, r) -> to_param_opt l r)
  |> List.sort ~compare:(Comparable.lift Int.compare ~f:fst)
  |> List.map ~f:snd

let sorted_params (vars : Var_map.t) :
    (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t =
  vars |> Common.Scoped_map.to_litmus_id_map |> Map.to_alist
  |> to_sorted_params_opt

(** Adjusts the type of a parameter in a (ID, type) associative list by
    turning it into a pointer if it is global, and leaving it unchanged
    otherwise.

    This serves to make the type fit its position in the stub: pointers to
    the Litmus harness's variables if global, local variables otherwise. *)
let type_adjusted_param ((id, ty) : Common.Litmus_id.t * Fir.Type.t) :
    (Common.Litmus_id.t * Fir.Type.t) Or_error.t =
  Or_error.Let_syntax.(
    let%map ty' =
      if Common.Litmus_id.is_global id then Fir.Type.ref ty else Ok ty
    in
    (id, ty'))

(** Produces a list of sorted, type-adjusted parameters from [vars]. These
    are the parameters of the inner call, and need to be filtered to produce
    the other parameter/argument lists. *)
let sorted_type_adjusted_params (vars : Var_map.t) :
    (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t Or_error.t =
  vars |> sorted_params |> Tx.Or_error.combine_map ~f:type_adjusted_param

let thread_params :
       (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t
    -> (Common.C_id.t, Fir.Type.t) List.Assoc.t =
  List.filter_map ~f:(fun (id, ty) ->
      Option.map ~f:(fun id' -> (id', ty)) (Common.Litmus_id.as_global id) )

let local_decls (tid : int) :
       (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t
    -> (Common.C_id.t, Fir.Initialiser.t) List.Assoc.t =
  List.filter_map ~f:(fun (id, ty) ->
      if [%equal: int option] (Common.Litmus_id.tid id) (Some tid) then
        Some
          ( Common.Litmus_id.variable_name id
          , Fir.
              { Initialiser.ty
              ; (* TODO(@MattWindsor91): fix this properly. *)
                value= Fir.Constant.int 0 } )
      else None )

let inner_call_argument (lid : Common.Litmus_id.t) (ty : Fir.Type.t) :
    Fir.Expression.t =
  let id = Common.Litmus_id.variable_name lid in
  let tid = Common.C_named.make ~name:id ty in
  Fir.Expression.address (Fir.Address.on_address_of_typed_id tid)

let inner_call_arguments (tid : int) :
    (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t -> Fir.Expression.t list =
  List.filter_map ~f:(fun (lid, ty) ->
      if Common.Litmus_id.is_in_local_scope ~from:tid lid then
        Some (inner_call_argument lid ty)
      else None )

let inner_call_stm (tid : int) (function_id : Common.C_id.t)
    (all_params : (Common.Litmus_id.t, Fir.Type.t) List.Assoc.t) :
    unit Fir.Statement.t =
  let arguments = inner_call_arguments tid all_params in
  let call = Fir.Call.make ~function_id ~arguments () in
  Accessor.construct
    Fir.(Statement.prim' @> Prim_statement.procedure_call)
    call

let make_function_stub (vars : Var_map.t) ~(old_id : Common.C_id.t)
    ~(new_id : Common.C_id.t) :
    unit Fir.Function.t Common.C_named.t Or_error.t =
  (* TODO(@MattWindsor91): eventually, we'll have variables that don't
     propagate outside of the wrapper into Litmus; in that case, the function
     stub should pass in their initial values directly. *)
  Or_error.Let_syntax.(
    let%bind all_params = sorted_type_adjusted_params vars in
    let parameters = thread_params all_params in
    let%map tid = try_parse_program_id old_id in
    let body_decls = local_decls tid all_params in
    let body_stms = [inner_call_stm tid new_id all_params] in
    let thread = Fir.Function.make ~parameters ~body_decls ~body_stms () in
    Common.C_named.make thread ~name:old_id)

let make ({litmus_header; var_map; function_map} : Aux.t) :
    Fir.Litmus.Test.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind threads =
      function_map
      |> Map.to_alist ~key_order:`Increasing
      |> List.filter_map ~f:(fun (old_id, record) ->
             if Function_map.Record.is_thread_body record then
               Some (make_function_stub var_map ~old_id ~new_id:record.c_id)
             else None )
      |> Or_error.combine_errors
    in
    Fir.Litmus.Test.make ~header:litmus_header ~threads)

module Filter = struct
  let run (input : Plumbing.Input.t) (output : Plumbing.Output.t) :
      unit Or_error.t =
    Or_error.(
      input |> Aux.load >>= make
      >>= Utils.My_format.odump output (Fmt.vbox Litmus_c.Reify.pp_litmus))
end
