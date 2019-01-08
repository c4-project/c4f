(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel

let make_atomic_int_initialiser (value : Ast_basic.Constant.t) =
  (* NB: Apparently, we don't need ATOMIC_VAR_INIT here:
     every known C11 compiler can make do without it, and as a result
     it's obsolete as of C17. *)
  Mini.Initialiser.make ~ty:Mini.Type.atomic_int ~value ()
;;

(** [make_init_globals init] converts a Litmus initialiser list to
    a set of global variable declarations. *)
let make_init_globals (init : (string, Ast_basic.Constant.t) List.Assoc.t)
  : (Ast_basic.Identifier.t, Mini.Initialiser.t) List.Assoc.t =
  List.Assoc.map ~f:make_atomic_int_initialiser init
;;

let qualify_local : int -> Mini.Identifier.t -> Mini.Identifier.t =
  sprintf "t%d%s"
;;

let make_single_func_globals (tid : int) (func : Mini.Function.t)
  : Mini.Initialiser.t Mini.id_assoc =
  List.map
    ~f:(fun (k, v) -> (qualify_local tid k, v))
    (Mini.Function.body_decls func)
;;

let make_func_globals (funcs : Mini.Function.t list)
  : Mini.Initialiser.t Mini.id_assoc =
  funcs
  |> List.mapi ~f:make_single_func_globals
  |> List.concat
;;

let qualify_locals
    (tid : int)
    (locals : Mini.Initialiser.t Mini.id_assoc)
  : Mini.Statement.t -> Mini.Statement.t =
  Mini.Statement.On_identifiers.map
    ~f:(fun id ->
        if List.Assoc.mem locals ~equal:String.equal id
        then qualify_local tid id
        else id)
;;

let address_globals
    (locals : Mini.Initialiser.t Mini.id_assoc)
  : Mini.Statement.t -> Mini.Statement.t =
  Mini.Statement.On_addresses.map
    ~f:(fun addr ->
        if List.Assoc.mem locals ~equal:String.equal
            (Mini.Address.underlying_variable addr)
        then addr
        else Mini.Address.ref addr
      )
;;

let delitmus_stms
    (tid : int)
    (locals : Mini.Initialiser.t Mini.id_assoc)
    (stms : Mini.Statement.t list) =
  stms
  |> List.map ~f:(address_globals locals)
  |> List.map ~f:(qualify_locals tid locals)
;;

let delitmus_function (tid : int) (func : Mini.Function.t)
  : Mini.Function.t =
  let locals = Mini.Function.body_decls func in
  Mini.Function.map func
    ~parameters:(Fn.const [])
    ~body_decls:(Fn.const [])
    ~body_stms:(delitmus_stms tid locals)
;;

let delitmus_functions
  :  Mini.Function.t Mini.id_assoc
  -> Mini.Function.t Mini.id_assoc =
  List.mapi
    ~f:(fun tid (name, f) -> (name, delitmus_function tid f))
;;

let run (input : Mini.Litmus_ast.Validated.t)
  : Mini.Program.t Or_error.t =
  let init = Mini.Litmus_ast.Validated.init input in
  let raw_functions = Mini.Litmus_ast.Validated.programs input in
  let init_globals = make_init_globals init in
  let func_globals = make_func_globals (List.map ~f:snd raw_functions) in
  let globals = init_globals @ func_globals in
  let functions =
    delitmus_functions raw_functions
  in
  Or_error.return (Mini.Program.make ~globals ~functions)
;;
