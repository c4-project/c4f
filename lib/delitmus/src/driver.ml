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
  val id_qualifier : Ac.Litmus_id.t -> Ac.C_id.t option

  val post_qualifier :
       Act_c.Mini_litmus.Ast.Postcondition.t
    -> Act_c.Mini_litmus.Ast.Postcondition.t

  module Function : Function_rewriter.S

  (* TODO(@MattWindsor91): add other differences between drivers here. *)
end) =
struct
  let make_global (ctx : Context.t)
      (id : Ac.Litmus_id.t)
      : (Ac.C_id.t * C.Mini.Initialiser.t) Or_error.t =
    Or_error.Let_syntax.(
      let%bind cid = Context.lookup_and_require_global ~id ctx in
      let value = Context.lookup_initial_value ~id ctx in
      let%map ty = Context.lookup_type ~id ctx in
      (cid, C.Mini.Initialiser.make ~ty ?value ())
    )

  let make_globals (ctx : Context.t)
      : C.Mini_initialiser.t C.Mini_intf.id_assoc Or_error.t =
    ctx
    |> Context.globally_mapped_litmus_ids
    |> Set.to_list
    |> Tx.Or_error.combine_map ~f:(make_global ctx)

  let make_litmus_aux (input : C.Mini_litmus.Ast.Validated.t) :
      C.Mini.Constant.t Act_litmus.Aux.t =
    let postcondition =
      Option.map
        (C.Mini_litmus.Ast.Validated.postcondition input)
        ~f:B.post_qualifier
    in
    (* These _should_ be ok to pass through verbatim; they only use global
       variables. *)
    let init = C.Mini_litmus.Ast.Validated.init input in
    let locations = C.Mini_litmus.Ast.Validated.locations input in
    Act_litmus.Aux.make ?postcondition ~init ?locations ()

  let make_var_map (input : C.Mini_litmus.Ast.Validated.t) : Var_map.t =
    Var_map.of_set_with_qualifier
      (C.Mini_litmus.vars input)
      ~qualifier:B.id_qualifier

  let make_aux (input : C.Mini_litmus.Ast.Validated.t) : Aux.t =
    let var_map = make_var_map input in
    let litmus_aux = make_litmus_aux input in
    let num_threads =
      List.length (C.Mini_litmus.Ast.Validated.programs input)
    in
    Aux.make ~litmus_aux ~var_map ~num_threads ()

  let make_program (input : C.Mini_litmus.Ast.Validated.t) (context : Context.t) :
      C.Mini.Program.t Or_error.t =
    let raw_functions = C.Mini_litmus.Ast.Validated.programs input in
    Or_error.Let_syntax.(
      let%bind functions =
        B.Function.rewrite_all raw_functions ~context
      in
      let%map globals = make_globals context in
      C.Mini.Program.make ~globals ~functions)

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
     pointer parameters, so we scrape the global type context from there.
     In doing so, we reduce the pointer types back to value ones. *)

  let lift_global_type_alist :
    (Act_common.C_id.t, C.Mini.Type.t) List.Assoc.t
    -> (Act_common.Litmus_id.t, C.Mini.Type.t) List.Assoc.t Or_error.t =
      Tx.Alist.With_errors.bi_map_m
        ~left:(Fn.compose Or_error.return Act_common.Litmus_id.global)
              ~right:Act_c.Mini.Type.deref

  let make_global_type_alist :
         C.Mini.Function.t list
      -> (Act_common.Litmus_id.t, C.Mini.Type.t) List.Assoc.t Or_error.t = function
    | [] ->
        Or_error.error_string "need at least one function"
    | x :: xs ->
        let params = C.Mini.Function.parameters x in
        Or_error.(
        xs
        |> List.map ~f:(check_parameters_consistent params)
        |> Or_error.combine_errors_unit
        >>= fun () -> lift_global_type_alist params)

  let make_local_type_alist (tid : int) (f : C.Mini.Function.t)
      : (Act_common.Litmus_id.t, Act_c.Mini.Type.t) List.Assoc.t =
    f
    |> C.Mini.Function.body_decls
    |> Tx.Alist.bi_map
         ~left:(Act_common.Litmus_id.local tid)
         ~right:C.Mini.Initialiser.ty

  let make_local_type_alists : C.Mini.Function.t list ->
      (Act_common.Litmus_id.t, Act_c.Mini.Type.t) List.Assoc.t list =
    List.mapi ~f:make_local_type_alist

  let make_type_map (functions : C.Mini.Function.t list)
      : Act_c.Mini.Type.t Map.M(Act_common.Litmus_id).t Or_error.t =
    Or_error.Let_syntax.(
      let%bind global_alist = make_global_type_alist functions in
      let local_alists = make_local_type_alists functions in
      let alist = List.concat (global_alist :: local_alists) in
      Map.of_alist_or_error (module Act_common.Litmus_id) alist
    )

  let make_local_init (fn : C.Mini.Function.t) :
    (Act_common.C_id.t, C.Mini.Constant.t) List.Assoc.t =
    fn
    |> C.Mini.Function.body_decls
    |> List.filter_map ~f:(fun (id, init) ->
      Option.(init |> C.Mini.Initialiser.value >>| fun v -> (id, v)))

  let make_local_inits : C.Mini.Function.t list ->
    (int, (Act_common.C_id.t, C.Mini.Constant.t) List.Assoc.t) List.Assoc.t =
    List.mapi ~f:(fun tid fn -> (tid, make_local_init fn))

  let make_context (input : C.Mini_litmus.Ast.Validated.t) (aux : Aux.t)
      : Context.t Or_error.t =
    (* We can get the context just from looking at functions, because of
       the way in which C litmus tests are constructed. *)
    let functions = List.map ~f:snd 
      (C.Mini_litmus.Ast.Validated.programs input) in
    let local_inits = make_local_inits functions in
    Or_error.Let_syntax.(
      let%map type_map = make_type_map functions in
      Context.make ~aux ~type_map ~local_inits
    )

  let run (input : C.Mini_litmus.Ast.Validated.t) : Output.t Or_error.t =
    let aux = make_aux input in
    Or_error.Let_syntax.(
      let%bind context = make_context input aux in
      let%map program = make_program input context in
      Output.make ~program ~aux)
end

module Vars_as_globals = Make (struct
  let id_qualifier x = Some (Qualify.litmus_id x)

  let post_qualifier = Qualify.postcondition

  module Function = Function_rewriter.Vars_as_globals
end)

module Vars_as_parameters = Make (struct
  let id_qualifier _ = None

  let post_qualifier = Fn.id

  module Function = Function_rewriter.Vars_as_parameters
end)
