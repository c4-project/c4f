(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
module Ac = Act_common
module Tx = Travesty_base_exts

let record_to_constant (r : Ac.C_variables.Record.t)
    ~(of_int : int -> 'const) : 'const =
  r |> Ac.C_variables.Record.initial_value |> Option.value ~default:0
  |> of_int

let make_init_from_vars (cvars : Ac.C_variables.Map.t)
    ~(of_int : int -> 'const) : (Ac.C_id.t, 'const) List.Assoc.t =
  cvars |> Ac.C_id.Map.to_alist
  |> Tx.Alist.bi_map ~left:Fn.id ~right:(record_to_constant ~of_int)

let make_init_from_all_heap_symbols (heap_syms : Act_abstract.Symbol.Set.t)
    ~(of_int : int -> 'const) : (Ac.C_id.t, 'const) List.Assoc.t =
  heap_syms |> Act_abstract.Symbol.Set.to_list
  |> List.map ~f:(fun s -> (Ac.C_id.of_string s, of_int 0))

let make_init ?(c_variables : Ac.C_variables.Map.t option)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) ~(of_int : int -> 'const) ()
    : (Ac.C_id.t, 'const) List.Assoc.t =
  c_variables
  |> Option.map ~f:(make_init_from_vars ~of_int)
  |> Tx.Option.value_f ~default_f:(fun () ->
         make_init_from_all_heap_symbols heap_symbols ~of_int )

let make_locations_from_config (cvars : Ac.C_variables.Map.t) :
    Set.M(Ac.C_id).t =
  cvars |> Ac.C_variables.Map.globals

let make_locations_from_init (init : (Ac.C_id.t, _) List.Assoc.t) :
    Set.M(Ac.C_id).t =
  init |> List.map ~f:fst |> Set.of_list (module Ac.C_id)

let make_global_locations ?(c_variables : Ac.C_variables.Map.t option)
    ~(init : (Ac.C_id.t, _) List.Assoc.t) () : Set.M(Ac.C_id).t =
  match c_variables with
  | Some cvars ->
      make_locations_from_config cvars
  | None ->
      make_locations_from_init init

let add_postcondition_locations (type const)
    (postcondition : const Act_litmus.Ast_base.Postcondition.t)
    ~(globals : Set.M(Ac.C_id).t) : Set.M(Ac.C_id).t =
  let module It = Act_litmus.Ast_base.Postcondition.On_identifiers (struct
    type t = const
  end) in
  let pc_ids = It.to_list postcondition in
  let pc_c_ids = List.filter_map ~f:Act_common.Litmus_id.as_global pc_ids in
  Set.(union globals (of_list (module Ac.C_id) pc_c_ids))

let try_add_postcondition_locations
    ?(postcondition : 'const Act_litmus.Ast_base.Postcondition.t option)
    (globals : Set.M(Ac.C_id).t) : Set.M(Ac.C_id).t =
  Option.value_map postcondition ~default:globals
    ~f:(add_postcondition_locations ~globals)

let make_locations
    ?(postcondition : 'const Act_litmus.Ast_base.Postcondition.t option)
    ?(c_variables : Ac.C_variables.Map.t option)
    ~(init : (Ac.C_id.t, _) List.Assoc.t) () : Ac.C_id.t list =
  let globals = make_global_locations ?c_variables ~init () in
  let with_pc = try_add_postcondition_locations ?postcondition globals in
  Set.to_list with_pc

let is_live_symbol (cid : Ac.C_id.t)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) =
  Act_abstract.Symbol.Set.mem heap_symbols (Ac.C_id.to_string cid)

let live_symbols_only (c_variables : Ac.C_variables.Map.t)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) : Ac.C_variables.Map.t =
  Ac.C_id.Map.filter_keys c_variables ~f:(is_live_symbol ~heap_symbols)

let make ?(c_variables : Ac.C_variables.Map.t option)
    ?(postcondition : 'const Act_litmus.Ast_base.Postcondition.t option)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) ~(of_int : int -> 'const) :
    'const Act_litmus.Aux.t =
  let live_cvars_opt =
    Option.map ~f:(live_symbols_only ~heap_symbols) c_variables
  in
  let init =
    make_init ?c_variables:live_cvars_opt ~heap_symbols ~of_int ()
  in
  let locations =
    make_locations ?postcondition ?c_variables:live_cvars_opt ~init ()
  in
  Act_litmus.Aux.make ~locations ~init ?postcondition ()
