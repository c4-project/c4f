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
include Litmusifier_intf
module Ac = Act_common
module Tx = Travesty_core_kernel_exts

type 'const t =
  { locations: Ac.C_id.t list option
  ; init: (Ac.C_id.t, 'const) List.Assoc.t
  ; postcondition: 'const Act_litmus.Ast_base.Postcondition.t option }
[@@deriving fields, make]

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
    Ac.C_id.t list =
  cvars |> Ac.C_variables.Map.globals |> Set.to_list

let make_locations_from_init (init : (Ac.C_id.t, _) List.Assoc.t) :
    Ac.C_id.t list =
  List.map ~f:fst init

let make_locations ?(c_variables : Ac.C_variables.Map.t option)
    ~(init : (Ac.C_id.t, _) List.Assoc.t) () : Ac.C_id.t list =
  match c_variables with
  | Some cvars ->
      make_locations_from_config cvars
  | None ->
      make_locations_from_init init

let is_live_symbol (cid : Ac.C_id.t)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) =
  Act_abstract.Symbol.Set.mem heap_symbols (Ac.C_id.to_string cid)

let live_symbols_only (c_variables : Ac.C_variables.Map.t)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) : Ac.C_variables.Map.t =
  Ac.C_id.Map.filter_keys c_variables ~f:(is_live_symbol ~heap_symbols)

let make ?(c_variables : Ac.C_variables.Map.t option)
    ?(postcondition : 'const Act_litmus.Ast_base.Postcondition.t option)
    ~(heap_symbols : Act_abstract.Symbol.Set.t) ~(of_int : int -> 'const) :
    'const t =
  let live_cvars_opt =
    Option.map ~f:(live_symbols_only ~heap_symbols) c_variables
  in
  let init =
    make_init ?c_variables:live_cvars_opt ~heap_symbols ~of_int ()
  in
  let locations = make_locations ?c_variables:live_cvars_opt ~init () in
  make ~locations ~init ?postcondition ()
