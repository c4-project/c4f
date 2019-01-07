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

(** [make_init_global name value] converts a single Litmus initialiser
    to a global variable declaration. *)
let make_init_global (name : string) (value : Ast_basic.Constant.t)
  : Ast.External_decl.t Or_error.t =
  Or_error.return
    Ast.(
      `Decl
        { Decl.qualifiers = [ `Int ]
        ; declarator = [ { declarator =
                             { pointer = None
                             ; direct = Id name
                             }
                         ; initialiser = Some (Assign (Constant value))
                         }
                       ]
        }
    )
;;

(** [make_init_globals init] converts a Litmus initialiser list to
    a set of global variable declarations. *)
let make_init_globals (init : (string, Ast_basic.Constant.t) List.Assoc.t)
  : Ast.External_decl.t list Or_error.t =
  init
  |> List.map ~f:(Tuple2.uncurry make_init_global)
  |> Or_error.combine_errors
;;

let run (input : Ast.Litmus.Validated.t)
  : Ast.Translation_unit.t Or_error.t =
  let open Or_error.Let_syntax in
  let init = Ast.Litmus.Validated.init input in
  let%map init_globals = make_init_globals init in
  init_globals
;;
