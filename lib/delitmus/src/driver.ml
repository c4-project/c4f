(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

module type S = sig
  val run : Act_c_mini.Litmus.Test.t -> Output.t Or_error.t
end

module Make (B : sig
  val globals_become_globals : bool

  val locals_become_globals : bool

  val rewrite_function_name : Act_common.C_id.t ->
      Act_common.C_id.t Or_error.t

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
        ~f:Qualify.postcondition
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let name = Act_c_mini.Litmus.Test.name input in
    let init = Act_c_mini.Litmus.Test.init input in
    let locations = Act_c_mini.Litmus.Test.locations input in
    Act_litmus.Header.make ~name ?postcondition ~init ?locations ()

  let make_global (c_id : Ac.C_id.t) (ptr_type : Act_c_mini.Type.t) :
      Var_map.Record.t Or_error.t =
    let mapped_to_global = B.globals_become_globals in
    Or_error.Let_syntax.(
      (* Globals in a valid C litmus test come through as pointers. *)
      let%map c_type = Act_c_mini.Type.deref ptr_type in
      Var_map.Record.make ~c_type ~mapped_to_global ~c_id)

  let make_local (tid : int) (local_c_id : Ac.C_id.t)
      (c_type : Act_c_mini.Type.t) : Var_map.Record.t Or_error.t =
    let mapped_to_global = B.locals_become_globals in
    let lit_id = Act_common.Litmus_id.local tid local_c_id in
    let c_id = Qualify.litmus_id lit_id in
    Or_error.return (Var_map.Record.make ~c_type ~mapped_to_global ~c_id)

  let make_var_map : Act_c_mini.Litmus.Test.t -> Var_map.t Or_error.t =
    Act_c_mini.Litmus_vars.make_scoped_map ~make_global ~make_local

  let make_named_function_record (func : unit Act_c_mini.Function.t Act_c_mini.Named.t)
      : (Act_common.C_id.t * Function_map.Record.t) Or_error.t =
    let name = Act_c_mini.Named.name func in
    Or_error.Let_syntax.(
    let%map new_name= B.rewrite_function_name name in
    (name,
    (* TODO(@MattWindsor91): if we introduce non-thread-body functions,
       change this hard-coded 'true' flag. *)
    Function_map.Record.make ~is_thread_body:true ~c_id:new_name ()))

  let make_function_map (threads : unit Act_c_mini.Function.t Act_c_mini.Named.t list) : Function_map.t Or_error.t =
    Or_error.(
    threads
    |> Tx.Or_error.combine_map ~f:make_named_function_record
    >>= Map.of_alist_or_error (module Act_common.C_id)
  )

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
      let functions = Act_c_mini.Named.alist_of_list function_list in
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
      List.map ~f:Act_c_mini.Named.value
        (Act_c_mini.Litmus.Test.threads input)
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

module Vars_as_globals = Make (struct
  let globals_become_globals = true

  let locals_become_globals = true

  let rewrite_function_name : Act_common.C_id.t ->
      Act_common.C_id.t Or_error.t = Or_error.return

  module Function = Function_rewriter.Vars_as_globals
end)

module Vars_as_parameters = Make (struct
  let globals_become_globals = false

  let locals_become_globals = false

  let rewrite_function_name (fname : Act_common.C_id.t) :
      Act_common.C_id.t Or_error.t =
    (* TODO(@MattWindsor91): make this customisable? *)
    let fname_str = Act_common.C_id.to_string fname in
    let fname_str' = fname_str ^ "_body" in
    Act_common.C_id.create fname_str'

  module Function = Function_rewriter.Vars_as_parameters
end)
