(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let try_parse_program_id (id : Act_common.C_id.t) : int Or_error.t =
  let strid = Act_common.C_id.to_string id in
  Or_error.(
    tag ~tag:"Thread function does not have a well-formed name"
      (try_with (fun () -> Caml.Scanf.sscanf strid "P%d" Fn.id)))

let params_of_tid (vars : Var_map.t) (tid : int) :
    (Act_common.C_id.t, Act_c_mini.Type.t) List.Assoc.t =
  (* TODO(@MattWindsor91): we're very much assuming here that the parameters
     always appear in ascending string order. Does delitmus give those
     guarantees? *)
  vars
  |> Act_common.Scoped_map.to_c_id_map ~scope:(Act_common.Scope.Local tid)
  |> Map.to_alist ~key_order:`Increasing
  |> List.filter_map ~f:(fun (_, record) ->
         Option.some_if
           (not (Var_map.Record.mapped_to_global record))
           (Var_map.Record.c_id record, Var_map.Record.c_type record))

let make_function_stub (vars : Var_map.t) ~(old_id : Act_common.C_id.t)
    ~(new_id : Act_common.C_id.t) :
    unit Act_c_mini.Function.t Act_common.C_named.t Or_error.t =
  (* TODO(@MattWindsor91): eventually, we'll have variables that don't
     propagate outside of the wrapper into Litmus; in that case, the function
     stub should pass in their initial values directly. *)
  Or_error.Let_syntax.(
    let%map tid = try_parse_program_id old_id in
    let parameters = params_of_tid vars tid in
    let arguments =
      List.map parameters ~f:(fun (id, ty) ->
          Act_c_mini.Expression.lvalue
            (Act_c_mini.Lvalue.on_value_of_typed_id ~id ~ty))
    in
    let call =
      Act_c_mini.Call.make ~function_id:new_id ~metadata:() ~arguments ()
    in
    let body_stms = [Act_c_mini.Statement.procedure_call call] in
    let thread =
      Act_c_mini.Function.make ~parameters ~body_decls:[] ~body_stms ()
    in
    Act_common.C_named.make thread ~name:old_id)

let make (aux : Aux.t) : Act_c_mini.Litmus.Test.t Or_error.t =
  let header = Aux.litmus_header aux in
  let vars = Aux.var_map aux in
  Or_error.Let_syntax.(
    let%bind threads =
      aux |> Aux.function_map
      |> Map.to_alist ~key_order:`Increasing
      |> List.filter_map ~f:(fun (old_id, record) ->
             if Function_map.Record.is_thread_body record then
               Some
                 (make_function_stub vars ~old_id
                    ~new_id:(Function_map.Record.c_id record))
             else None)
      |> Or_error.combine_errors
    in
    Act_c_mini.Litmus.Test.make ~header ~threads)

module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit =
Plumbing.Filter.Make (struct
  type aux_i = unit

  type aux_o = unit

  let name = "make-stub"

  let run (ctx : aux_i Plumbing.Filter_context.t) (ic : Stdio.In_channel.t)
      (oc : Stdio.Out_channel.t) : aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%bind aux =
        Aux.load_from_ic
          ~path:(Plumbing.Filter_context.input_path_string ctx)
          ic
      in
      let%map stub = make aux in
      Act_c_mini.Litmus.Pp.print oc stub)
end)
