(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t =
  { aux: Aux.t
  ; local_inits:
      (int, (Common.C_id.t, Fir.Constant.t) List.Assoc.t) List.Assoc.t }
[@@deriving fields]

let make = Fields.create

let var_map (ctx : t) : Var_map.t = (aux ctx).var_map

let lookup_initial_value_global (ctx : t) ~(id : Common.C_id.t) :
    Fir.Constant.t option =
  let init = (aux ctx).litmus_header |> Act_litmus.Header.init in
  List.Assoc.find ~equal:Common.C_id.equal init id

let lookup_initial_value_local (ctx : t) ~(tid : int) ~(id : Common.C_id.t) :
    Fir.Constant.t option =
  Option.(
    List.Assoc.find ~equal:Int.equal (local_inits ctx) tid
    >>= fun init -> List.Assoc.find ~equal:Common.C_id.equal init id)

let lookup_initial_value (ctx : t) ~(id : Common.Litmus_id.t) :
    Fir.Constant.t option =
  match Common.Litmus_id.as_local id with
  | Some (tid, id) ->
      lookup_initial_value_local ctx ~tid ~id
  | None ->
      Option.bind (Common.Litmus_id.as_global id) ~f:(fun id ->
          lookup_initial_value_global ctx ~id)
