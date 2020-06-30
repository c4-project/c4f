(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
end

module Make (B : Runner_types.Basic) = struct
  let make_litmus_header (input : Act_fir.Litmus.Test.t) :
      Act_fir.Constant.t Act_litmus.Header.t =
    let postcondition =
      Option.map
        (Act_fir.Litmus.Test.postcondition input)
        ~f:(Qualify.postcondition ~qualify_locals:B.qualify_locals)
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let name = Act_fir.Litmus.Test.name input in
    let init = Act_fir.Litmus.Test.init input in
    let locations = Act_fir.Litmus.Test.locations input in
    Act_litmus.Header.make ~name ?postcondition ~init ?locations ()

  let make_var_record (index : int) (id : Ac.Litmus_id.t)
      (orig_type : Act_fir.Type.t) : Var_map.Record.t Or_error.t =
    let is_global = Ac.Litmus_id.is_global id in
    let mapped_to =
      (if is_global then B.global_mapping else B.local_mapping) index
    in
    let c_id = Qualify.litmus_id ~qualify_locals:B.qualify_locals id in
    Or_error.Let_syntax.(
      let%map c_type =
        (* Globals in a valid C litmus test come through as pointers. *)
        if is_global then Act_fir.Type.deref orig_type
        else Or_error.return orig_type
      in
      Var_map.Record.make ~c_type ~mapped_to ~c_id)

  let make_var_map (test : Act_fir.Litmus.Test.t) : Var_map.t Or_error.t =
    Or_error.(
      test |> Act_fir.Litmus_vars.make_type_alist
      >>| List.mapi ~f:(fun index (id, ty) ->
              make_var_record index id ty >>| fun rc -> (id, rc) )
      >>= Or_error.combine_errors
      >>= Map.of_alist_or_error (module Ac.Litmus_id)
      >>| Ac.Scoped_map.of_litmus_id_map)

  let rewrite_function_name (name : Ac.C_id.t) : Ac.C_id.t Or_error.t =
    Option.value_map B.impl_suffix
      ~f:(fun suffix -> Ac.C_id.create (Ac.C_id.to_string name ^ suffix))
      ~default:(Or_error.return name)

  let make_named_function_record
      (func : unit Act_fir.Function.t Ac.C_named.t) :
      (Act_common.C_id.t * Function_map.Record.t) Or_error.t =
    let name = Ac.C_named.name func in
    Or_error.Let_syntax.(
      let%map new_name = rewrite_function_name name in
      ( name
      , (* TODO(@MattWindsor91): if we introduce non-thread-body functions,
           change this hard-coded 'true' flag. *)
        Function_map.Record.make ~is_thread_body:true ~c_id:new_name () ))

  let make_function_map (threads : unit Act_fir.Function.t Ac.C_named.t list)
      : Function_map.t Or_error.t =
    Or_error.(
      threads
      |> Tx.Or_error.combine_map ~f:make_named_function_record
      >>= Map.of_alist_or_error (module Act_common.C_id))

  let make_aux (input : Act_fir.Litmus.Test.t) : Aux.t Or_error.t =
    let threads = Act_fir.Litmus.Test.threads input in
    let litmus_header = make_litmus_header input in
    Or_error.Let_syntax.(
      let%bind function_map = make_function_map threads in
      let%map var_map = make_var_map input in
      Aux.make ~litmus_header ~function_map ~var_map ())
end
