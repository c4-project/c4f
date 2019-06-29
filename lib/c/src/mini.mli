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

(** A miniature model of a memalloy-witness-style C program.

    Unlike {{!Ast} Ast}, which tries to be a fairly faithful C abstract
    syntax tree, this module describes a tiny subset of C that maps well to
    litmus tests, and does so in a fairly high-level manner.

    One can get a 'mini' C program by {{!Convert} converting} an AST to it
    (which may fail). To get an AST (for printing, etc.), use
    {{!Mini_reify} Reify}. *)

module Constant = Act_c_lang.Ast_basic.Constant
module Identifier = Act_c_lang.Ast_basic.Identifier
module Pointer = Act_c_lang.Ast_basic.Pointer

module Type = Mini_type
(** Re-exporting {{!Mini_type} Type}. *)

module Initialiser = Mini_initialiser
(** Re-exporting {{!Mini_initialiser} Initialiser}. *)

module Lvalue = Mini_lvalue
(** Re-exporting {{!Mini_lvalue} Lvalue}. *)

module Address = Mini_address
(** Re-exporting {{!Mini_address} Address}. *)

module Expression = Mini_expression
(** Re-exporting {{!Mini_expression} Expression}. *)

module Atomic_cmpxchg = Mini_atomic_cmpxchg
(** Re-exporting {{!Mini_atomic_cmpxchg} Atomic_cmpxchg}. *)

module Atomic_load = Mini_atomic_load
(** Re-exporting {{!Mini_atomic_load} Atomic_load}. *)

module Atomic_store = Mini_atomic_store
(** Re-exporting {{!Mini_atomic_store} Atomic_store}. *)

module Assign = Mini_assign
(** Re-exporting {{!Mini_assign} Assign}. *)

module Statement = Mini_statement
(** Re-exporting {{!Mini_statement} Statement}. *)

module Function = Mini_function
(** Re-exporting {{!Mini_function} Function}. *)

module Program = Mini_program
(** Re-exporting {{!Mini_program} Program}. *)
