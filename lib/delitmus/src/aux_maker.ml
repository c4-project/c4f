(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Make (B : Runner_types.Basic) = struct
  let make_litmus_header (input : Fir.Litmus.Test.t) :
      Fir.Constant.t Act_litmus.Header.t =
    let postcondition =
      Option.map
        (Fir.Litmus.Test.postcondition input)
        ~f:(Qualify.postcondition ~qualify_locals:B.qualify_locals)
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let name = Fir.Litmus.Test.name input in
    let init = Fir.Litmus.Test.init input in
    let locations = Fir.Litmus.Test.locations input in
    Act_litmus.Header.make ~name ?postcondition ~init ?locations ()

  let make_var_record (index : int) (id : Common.Litmus_id.t)
      (orig_type : Fir.Type.t) : Var_map.Record.t Or_error.t =
    let is_global = Common.Litmus_id.is_global id in
    let mapped_to =
      (if is_global then B.global_mapping else B.local_mapping) index
    in
    let c_id = Qualify.litmus_id ~qualify_locals:B.qualify_locals id in
    Or_error.Let_syntax.(
      let%map c_type =
        (* Globals in a valid C litmus test come through as pointers. *)
        if is_global then Fir.Type.deref orig_type
        else Ok orig_type
      in
      Var_map.Record.make ~c_type ~mapped_to ~c_id)

  let make_var_map (test : Fir.Litmus.Test.t) : Var_map.t Or_error.t =
    Or_error.(
      test |> Fir.Litmus_vars.make_type_alist
      >>| List.mapi ~f:(fun index (id, ty) ->
              make_var_record index id ty >>| fun rc -> (id, rc))
      >>= Or_error.combine_errors
      >>= Map.of_alist_or_error (module Common.Litmus_id)
      >>| Common.Scoped_map.of_litmus_id_map)

  let rewrite_function_name (name : Common.C_id.t) : Common.C_id.t Or_error.t
      =
    Option.value_map B.impl_suffix
      ~f:(fun suffix ->
        Common.C_id.create (Common.C_id.to_string name ^ suffix))
      ~default:(Or_error.return name)

  let make_named_function_record
      (func : unit Fir.Function.t Common.C_named.t) :
      (Common.C_id.t * Function_map.Record.t) Or_error.t =
    let name = Accessor.get Common.C_named.name func in
    Or_error.Let_syntax.(
      let%map new_name = rewrite_function_name name in
      ( name
      , (* TODO(@MattWindsor91): if we introduce non-thread-body functions,
           change this hard-coded 'true' flag. *)
        Function_map.Record.make ~is_thread_body:true ~c_id:new_name () ))

  let make_function_map (threads : unit Fir.Function.t Common.C_named.t list)
      : Function_map.t Or_error.t =
    Or_error.(
      threads
      |> Tx.Or_error.combine_map ~f:make_named_function_record
      >>= Map.of_alist_or_error (module Common.C_id))

  let make_aux (input : Fir.Litmus.Test.t) : Aux.t Or_error.t =
    let threads = Fir.Litmus.Test.threads input in
    let litmus_header = make_litmus_header input in
    Or_error.Let_syntax.(
      let%bind function_map = make_function_map threads in
      let%map var_map = make_var_map input in
      Aux.make ~litmus_header ~function_map ~var_map ())
end
