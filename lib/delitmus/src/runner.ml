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

let make_initialiser ((ty, value) : C.Mini.Type.t * C.Mini.Constant.t) =
  (* NB: Apparently, we don't need ATOMIC_VAR_INIT here: every known C11
     compiler can make do without it, and as a result it's obsolete as of
     C17. *)
  C.Mini.Initialiser.make ~ty ~value ()

let parameter_list_equal :
       C.Mini.Type.t C.Mini_intf.id_assoc
    -> C.Mini.Type.t C.Mini_intf.id_assoc
    -> bool =
  [%equal: (Ac.C_id.t * C.Mini.Type.t) list]

let check_parameters_consistent
    (params : C.Mini.Type.t C.Mini_intf.id_assoc) (next : C.Mini.Function.t)
    : unit Or_error.t =
  let params' = C.Mini.Function.parameters next in
  if parameter_list_equal params params' then Result.ok_unit
  else
    Or_error.error_s
      [%message
        "Functions do not agree on parameter lists"
          ~first_example:(params : C.Mini.Type.t C.Mini_intf.id_assoc)
          ~second_example:(params' : C.Mini.Type.t C.Mini_intf.id_assoc)]

let functions_to_parameter_map :
    C.Mini.Function.t list -> C.Mini.Type.t C.Mini_intf.id_assoc Or_error.t
    = function
  | [] ->
      Or_error.error_string "need at least one function"
  | x :: xs ->
      let open Or_error.Let_syntax in
      let params = C.Mini.Function.parameters x in
      let%map () =
        xs
        |> List.map ~f:(check_parameters_consistent params)
        |> Or_error.combine_errors_unit
      in
      params

let merge_init_and_params (init : C.Mini.Constant.t C.Mini_intf.id_assoc)
    (params : C.Mini.Type.t C.Mini_intf.id_assoc) :
    (C.Mini.Type.t * C.Mini.Constant.t) C.Mini_intf.id_assoc Or_error.t =
  let i_ids = init |> List.map ~f:fst |> Ac.C_id.Set.of_list in
  let p_ids = params |> List.map ~f:fst |> Ac.C_id.Set.of_list in
  if Ac.C_id.Set.equal i_ids p_ids then
    params
    |> List.map ~f:(fun (id, ty) ->
           (id, (ty, List.Assoc.find_exn ~equal:Ac.C_id.equal init id)) )
    |> Or_error.return
  else
    Or_error.error_s
      [%message
        "Init and parameters lists don't agree"
          ~init_ids:(i_ids : Ac.C_id.Set.t)
          ~param_ids:(p_ids : Ac.C_id.Set.t)]

(** [dereference_params params] tries to convert each parameter in [params]
    from a pointer type to a non-pointer type. It fails if any of the
    parameters are non-pointer types.

    Since we assume that litmus tests contain pointers to the global
    variables in their parameter lists, failure of this function generally
    means the litmus test being delitmusified is ill-formed. *)
let dereference_params (params : C.Mini.Type.t C.Mini_intf.id_assoc) :
    C.Mini.Type.t C.Mini_intf.id_assoc Or_error.t =
  Or_error.(
    params
    |> List.map ~f:(fun (id, ty) ->
           ty |> C.Mini.Type.deref >>| fun ty' -> (id, ty') )
    |> combine_errors)

(** [make_init_globals init functions] converts a Litmus initialiser list to
    a set of global variable declarations, using the type information from
    [functions].

    It fails if the functions list is empty, is inconsistent with its
    parameters, or the parameters don't match the initialiser. *)
let make_init_globals (init : C.Mini.Constant.t C.Mini_intf.id_assoc)
    (functions : C.Mini.Function.t list) :
    (C.Mini.Identifier.t, C.Mini.Initialiser.t) List.Assoc.t Or_error.t =
  Or_error.(
    functions |> functions_to_parameter_map >>= dereference_params
    >>= merge_init_and_params init
    >>| List.Assoc.map ~f:make_initialiser)

let make_single_func_globals (tid : int) (func : C.Mini.Function.t) :
    C.Mini.Initialiser.t C.Mini_intf.id_assoc =
  Tx.Alist.map_left ~f:(Qualify.local tid) (C.Mini.Function.body_decls func)

let make_func_globals (funcs : C.Mini.Function.t list) :
    C.Mini.Initialiser.t C.Mini_intf.id_assoc =
  funcs |> List.mapi ~f:make_single_func_globals |> List.concat

let make_globals (init : C.Mini.Constant.t C.Mini_intf.id_assoc)
    (function_bodies : C.Mini.Function.t list) :
    C.Mini_initialiser.t C.Mini_intf.id_assoc Or_error.t =
  let func_globals = make_func_globals function_bodies in
  Or_error.Let_syntax.(
    let%map init_globals = make_init_globals init function_bodies in
    init_globals @ func_globals)

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

let make_var_map (input : C.Mini_litmus.Ast.Validated.t)
    ~(qualifier : Ac.Litmus_id.t -> Ac.C_id.t option) : Var_map.t =
  Var_map.of_set_with_qualifier (C.Mini_litmus.vars input) ~qualifier

let make_aux (input : C.Mini_litmus.Ast.Validated.t)
    ~(qualifier : Ac.Litmus_id.t -> Ac.C_id.t option) : Aux.t =
  let var_map = make_var_map input ~qualifier in
  let litmus_aux = make_litmus_aux input in
  let num_threads =
    List.length (C.Mini_litmus.Ast.Validated.programs input)
  in
  Aux.make ~litmus_aux ~var_map ~num_threads ()

let make_program (input : C.Mini_litmus.Ast.Validated.t) (aux : Aux.t) :
    C.Mini.Program.t Or_error.t =
  let raw_functions = C.Mini_litmus.Ast.Validated.programs input in
  let function_bodies = List.map ~f:snd raw_functions in
  let functions = Function_rewriter.rewrite_all raw_functions in
  let init = Act_litmus.Aux.init (Aux.litmus_aux aux) in
  Or_error.Let_syntax.(
    let%map globals = make_globals init function_bodies in
    C.Mini.Program.make ~globals ~functions)

let run (input : C.Mini_litmus.Ast.Validated.t) : Output.t Or_error.t =
  let qualifier x = Some (Qualify.litmus_id x) in
  let aux = make_aux input ~qualifier in
  Or_error.Let_syntax.(
    let%map program = make_program input aux in
    Output.make ~program ~aux)
