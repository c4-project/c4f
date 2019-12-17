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

module type S = sig
  val run : Act_c_mini.Litmus.Test.t -> Output.t Or_error.t
end

module Make (B : sig
  val global_mapping : int -> Var_map.Mapping.t

  val local_mapping : int -> Var_map.Mapping.t

  val impl_suffix : string option

  val qualify_locals : bool

  module Function : Function_rewriter.S
end) =
struct
  let make_global (ctx : Context.t) (id : Ac.Litmus_id.t)
      (record : Var_map.Record.t) : Ac.C_id.t * Act_c_mini.Initialiser.t =
    let value = Context.lookup_initial_value ~id ctx in
    let cid = Var_map.Record.c_id record in
    let ty = Var_map.Record.c_type record in
    (cid, Act_c_mini.Initialiser.make ~ty ?value ())

  let make_globals (ctx : Context.t) :
      (Act_common.C_id.t, Act_c_mini.Initialiser.t) List.Assoc.t =
    let vm = Context.var_map ctx in
    let global_recs = Var_map.globally_mapped_vars vm in
    List.map ~f:(fun (i, v) -> make_global ctx i v) global_recs

  let make_litmus_header (input : Act_c_mini.Litmus.Test.t) :
      Act_c_mini.Constant.t Act_litmus.Header.t =
    let postcondition =
      Option.map
        (Act_c_mini.Litmus.Test.postcondition input)
        ~f:(Qualify.postcondition ~qualify_locals:B.qualify_locals)
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let name = Act_c_mini.Litmus.Test.name input in
    let init = Act_c_mini.Litmus.Test.init input in
    let locations = Act_c_mini.Litmus.Test.locations input in
    Act_litmus.Header.make ~name ?postcondition ~init ?locations ()

  let make_var_record (index : int) (id : Ac.Litmus_id.t)
      (orig_type : Act_c_mini.Type.t) : Var_map.Record.t Or_error.t =
    let is_global = Ac.Litmus_id.is_global id in
    let mapped_to =
      (if is_global then B.global_mapping else B.local_mapping) index
    in
    let c_id = Qualify.litmus_id ~qualify_locals:B.qualify_locals id in
    Or_error.Let_syntax.(
      let%map c_type =
        (* Globals in a valid C litmus test come through as pointers. *)
        if is_global then Act_c_mini.Type.deref orig_type
        else Or_error.return orig_type
      in
      Var_map.Record.make ~c_type ~mapped_to ~c_id)

  let make_var_map (test : Act_c_mini.Litmus.Test.t) : Var_map.t Or_error.t =
    Or_error.(
      test |> Act_c_mini.Litmus_vars.make_type_alist
      >>| List.mapi ~f:(fun index (id, ty) ->
              make_var_record index id ty >>| fun rc -> (id, rc))
      >>= Or_error.combine_errors
      >>= Map.of_alist_or_error (module Ac.Litmus_id)
      >>| Ac.Scoped_map.of_litmus_id_map)

  let rewrite_function_name (name : Ac.C_id.t) : Ac.C_id.t Or_error.t =
    Option.value_map B.impl_suffix
      ~f:(fun suffix -> Ac.C_id.create (Ac.C_id.to_string name ^ suffix))
      ~default:(Or_error.return name)

  let make_named_function_record
      (func : unit Act_c_mini.Function.t Ac.C_named.t) :
      (Act_common.C_id.t * Function_map.Record.t) Or_error.t =
    let name = Ac.C_named.name func in
    Or_error.Let_syntax.(
      let%map new_name = rewrite_function_name name in
      ( name
      , (* TODO(@MattWindsor91): if we introduce non-thread-body functions,
           change this hard-coded 'true' flag. *)
        Function_map.Record.make ~is_thread_body:true ~c_id:new_name () ))

  let make_function_map
      (threads : unit Act_c_mini.Function.t Ac.C_named.t list) :
      Function_map.t Or_error.t =
    Or_error.(
      threads
      |> Tx.Or_error.combine_map ~f:make_named_function_record
      >>= Map.of_alist_or_error (module Act_common.C_id))

  let make_aux (input : Act_c_mini.Litmus.Test.t) : Aux.t Or_error.t =
    let threads = Act_c_mini.Litmus.Test.threads input in
    let litmus_header = make_litmus_header input in
    Or_error.Let_syntax.(
      let%bind function_map = make_function_map threads in
      let%map var_map = make_var_map input in
      Aux.make ~litmus_header ~function_map ~var_map ())

  let make_program (input : Act_c_mini.Litmus.Test.t) (context : Context.t) :
      unit Act_c_mini.Program.t Or_error.t =
    let raw_functions = Act_c_mini.Litmus.Test.threads input in
    let globals = make_globals context in
    Or_error.Let_syntax.(
      let%map function_list =
        B.Function.rewrite_all raw_functions ~context
      in
      let functions = Ac.C_named.alist_of_list function_list in
      Act_c_mini.Program.make ~globals ~functions)

  let make_local_init (fn : unit Act_c_mini.Function.t) :
      (Act_common.C_id.t, Act_c_mini.Constant.t) List.Assoc.t =
    fn |> Act_c_mini.Function.body_decls
    |> List.filter_map ~f:(fun (id, init) ->
           Option.(init |> Act_c_mini.Initialiser.value >>| fun v -> (id, v)))

  let make_local_inits :
         unit Act_c_mini.Function.t list
      -> ( int
         , (Act_common.C_id.t, Act_c_mini.Constant.t) List.Assoc.t )
         List.Assoc.t =
    List.mapi ~f:(fun tid fn -> (tid, make_local_init fn))

  let make_context (input : Act_c_mini.Litmus.Test.t) (aux : Aux.t) :
      Context.t =
    (* We can get the context just from looking at functions, because of the
       way in which C litmus tests are constructed. *)
    let functions =
      List.map ~f:Ac.C_named.value (Act_c_mini.Litmus.Test.threads input)
    in
    let local_inits = make_local_inits functions in
    Context.make ~aux ~local_inits

  let run (input : Act_c_mini.Litmus.Test.t) : Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind aux = make_aux input in
      let context = make_context input aux in
      let%map program = make_program input context in
      Output.make ~program ~aux)
end
