(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common
module C = Act_c
module Tx = Travesty_base_exts

module type S = [%import: (module Driver.S)]

module Make (B : sig
  val globals_become_globals : bool

  val locals_become_globals : bool

  module Function : Function_rewriter.S
end) =
struct
  let make_global (ctx : Context.t) (id : Ac.Litmus_id.t)
      (record : Var_map.Record.t) : Ac.C_id.t * C.Mini.Initialiser.t =
    let value = Context.lookup_initial_value ~id ctx in
    let cid = Var_map.Record.c_id record in
    let ty = Var_map.Record.c_type record in
    (cid, C.Mini.Initialiser.make ~ty ?value ())

  let make_globals (ctx : Context.t) :
      C.Mini_initialiser.t C.Mini_intf.id_assoc =
    let vm = Context.var_map ctx in
    let global_recs = Var_map.globally_mapped_vars vm in
    List.map ~f:(fun (i, v) -> make_global ctx i v) global_recs

  let make_litmus_aux (input : C.Mini_litmus.Ast.Validated.t) :
      C.Mini.Constant.t Act_litmus.Aux.t =
    let postcondition =
      Option.map
        (C.Mini_litmus.Ast.Validated.postcondition input)
        ~f:Qualify.postcondition
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let init = C.Mini_litmus.Ast.Validated.init input in
    let locations = C.Mini_litmus.Ast.Validated.locations input in
    Act_litmus.Aux.make ?postcondition ~init ?locations ()

  let parameter_list_equal :
         C.Mini.Type.t C.Mini_intf.id_assoc
      -> C.Mini.Type.t C.Mini_intf.id_assoc
      -> bool =
    [%equal: (Ac.C_id.t * C.Mini.Type.t) list]

  let check_parameters_consistent
      (params : C.Mini.Type.t C.Mini_intf.id_assoc)
      (next : C.Mini.Function.t) : unit Or_error.t =
    let params' = C.Mini.Function.parameters next in
    if parameter_list_equal params params' then Result.ok_unit
    else
      Or_error.error_s
        [%message
          "Functions do not agree on parameter lists"
            ~first_example:(params : C.Mini.Type.t C.Mini_intf.id_assoc)
            ~second_example:(params' : C.Mini.Type.t C.Mini_intf.id_assoc)]

  (* In a memalloy style litmus test, the globals start as fully typed
     pointer parameters, so we scrape the global type context from there. In
     doing so, we reduce the pointer types back to value ones. *)

  let lift_global_var_alist :
         (Act_common.C_id.t, C.Mini.Type.t) List.Assoc.t
      -> (Act_common.Litmus_id.t, Var_map.Record.t) List.Assoc.t Or_error.t
      =
    Tx.Or_error.combine_map ~f:(fun (c_id, ptr_type) ->
        let lit_id = Act_common.Litmus_id.global c_id in
        Or_error.Let_syntax.(
          let%map c_type = Act_c.Mini.Type.deref ptr_type in
          ( lit_id
          , Var_map.Record.make ~c_type ~is_global:B.globals_become_globals
              ~c_id )))

  let make_global_var_alist :
         C.Mini.Function.t list
      -> (Act_common.Litmus_id.t, Var_map.Record.t) List.Assoc.t Or_error.t
      = function
    | [] ->
        Or_error.error_string "need at least one function"
    | x :: xs ->
        let params = C.Mini.Function.parameters x in
        Or_error.(
          xs
          |> List.map ~f:(check_parameters_consistent params)
          |> Or_error.combine_errors_unit
          >>= fun () -> lift_global_var_alist params)

  let make_local_var_alist (tid : int) (f : C.Mini.Function.t) :
      (Act_common.Litmus_id.t, Var_map.Record.t) List.Assoc.t =
    f |> C.Mini.Function.body_decls
    |> List.map ~f:(fun (local_c_id, init) ->
           let lit_id = Act_common.Litmus_id.local tid local_c_id in
           let c_id = Qualify.litmus_id lit_id in
           let c_type = C.Mini.Initialiser.ty init in
           ( lit_id
           , Var_map.Record.make ~c_type ~is_global:B.locals_become_globals
               ~c_id ))

  let make_local_var_alists :
         C.Mini.Function.t list
      -> (Act_common.Litmus_id.t, Var_map.Record.t) List.Assoc.t list =
    List.mapi ~f:make_local_var_alist

  let make_var_map (functions : C.Mini.Function.t list) :
      Var_map.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind global_alist = make_global_var_alist functions in
      let local_alists = make_local_var_alists functions in
      let alist = List.concat (global_alist :: local_alists) in
      let%map map =
        Map.of_alist_or_error (module Act_common.Litmus_id) alist
      in
      Var_map.of_map map)

  let make_aux (input : C.Mini_litmus.Ast.Validated.t) : Aux.t Or_error.t =
    let programs = C.Mini_litmus.Ast.Validated.programs input in
    let litmus_aux = make_litmus_aux input in
    let num_threads = List.length programs in
    Or_error.Let_syntax.(
      let%map var_map = make_var_map (List.map ~f:snd programs) in
      Aux.make ~litmus_aux ~var_map ~num_threads ())

  let make_program (input : C.Mini_litmus.Ast.Validated.t)
      (context : Context.t) : C.Mini.Program.t Or_error.t =
    let raw_functions = C.Mini_litmus.Ast.Validated.programs input in
    let globals = make_globals context in
    Or_error.Let_syntax.(
      let%map functions = B.Function.rewrite_all raw_functions ~context in
      C.Mini.Program.make ~globals ~functions)

  let make_local_init (fn : C.Mini.Function.t) :
      (Act_common.C_id.t, C.Mini.Constant.t) List.Assoc.t =
    fn |> C.Mini.Function.body_decls
    |> List.filter_map ~f:(fun (id, init) ->
           Option.(init |> C.Mini.Initialiser.value >>| fun v -> (id, v)))

  let make_local_inits :
         C.Mini.Function.t list
      -> ( int
         , (Act_common.C_id.t, C.Mini.Constant.t) List.Assoc.t )
         List.Assoc.t =
    List.mapi ~f:(fun tid fn -> (tid, make_local_init fn))

  let make_context (input : C.Mini_litmus.Ast.Validated.t) (aux : Aux.t) :
      Context.t =
    (* We can get the context just from looking at functions, because of the
       way in which C litmus tests are constructed. *)
    let functions =
      List.map ~f:snd (C.Mini_litmus.Ast.Validated.programs input)
    in
    let local_inits = make_local_inits functions in
    Context.make ~aux ~local_inits

  let run (input : C.Mini_litmus.Ast.Validated.t) : Output.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind aux = make_aux input in
      let context = make_context input aux in
      let%map program = make_program input context in
      Output.make ~program ~aux)
end

module Vars_as_globals = Make (struct
  let globals_become_globals = true

  let locals_become_globals = true

  module Function = Function_rewriter.Vars_as_globals
end)

module Vars_as_parameters = Make (struct
  let globals_become_globals = false

  let locals_become_globals = false

  module Function = Function_rewriter.Vars_as_parameters
end)
