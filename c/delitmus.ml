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
  : (Ast_basic.Identifier.t, Mini.Initialiser.t) List.Assoc.t Or_error.t =
  init
  |> List.Assoc.map ~f:make_atomic_int_initialiser
  |> Or_error.return
;;

let run (input : Mini.Litmus_ast.Validated.t)
  : Mini.Program.t Or_error.t =
  let open Or_error.Let_syntax in
  let init = Mini.Litmus_ast.Validated.init input in
  let%map globals = make_init_globals init in
  Mini.Program.make ~globals ~functions:[]
;;
