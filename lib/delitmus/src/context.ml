(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { type_map : Act_c.Mini.Type.t Map.M(Act_common.Litmus_id).t
  ; aux : Aux.t
  ; local_inits : (int, ((Act_common.C_id.t, Act_c.Mini.Constant.t) List.Assoc.t)) List.Assoc.t
  } [@@deriving fields]

let make = Fields.create

let var_map (ctx : t) : Var_map.t = Aux.var_map (aux ctx)

let not_found_error (id : Act_common.Litmus_id.t) (trying_to_find : string) :
  Error.t =
  Error.of_lazy_t
    (lazy
      (Error.create_s
         [%message "Identifier not found in context."
             ~trying_to_find
             ~id:(id : Act_common.Litmus_id.t)
         ]))

let lookup_type (ctx : t) ~(id:Act_common.Litmus_id.t) : Act_c.Mini.Type.t Or_error.t
  =
  Result.of_option
    (Map.find (type_map ctx) id)
    ~error:(not_found_error id "type")

let lookup_global (ctx : t) : id:Act_common.Litmus_id.t -> Act_common.C_id.t Or_error.t =
  Var_map.lookup_and_require_global (Aux.var_map (aux ctx))

let lookup_initial_value_global (ctx : t) ~(id:Act_common.C_id.t) : Act_c.Mini.Constant.t option =
  let init = ctx |> aux |> Aux.litmus_aux |> Act_litmus.Aux.init in
  List.Assoc.find ~equal:Act_common.C_id.equal init id

let lookup_initial_value_local (ctx : t) ~(tid:int) ~(id:Act_common.C_id.t) : Act_c.Mini.Constant.t option =
  Option.(
    List.Assoc.find ~equal:Int.equal (local_inits ctx) tid
    >>= fun init -> List.Assoc.find ~equal:Act_common.C_id.equal init id
  )

let lookup_initial_value (ctx : t) ~(id:Act_common.Litmus_id.t) : Act_c.Mini.Constant.t option =
  match Act_common.Litmus_id.as_local id with
  | Some (tid, id) -> lookup_initial_value_local ctx ~tid ~id
  | None ->
    Option.bind (Act_common.Litmus_id.as_global id)
      ~f:(fun id -> lookup_initial_value_global ctx ~id)
