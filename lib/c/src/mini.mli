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
open Mini_intf

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

module Program : sig
  type t [@@deriving sexp]

  (** {3 Constructors} *)

  val make :
    globals:Initialiser.t id_assoc -> functions:Function.t id_assoc -> t
  (** [make ~globals ~functions] makes a program with global variable
      declarations [globals] and function definitions [functions]. *)

  (** {3 Accessors} *)

  val globals : t -> Initialiser.t id_assoc
  (** [globals program] gets an associative list of each global initialiser
      in [program]. *)

  val functions : t -> Function.t id_assoc
  (** [functions program] gets an associative list of each function in
      [program]. *)

  val cvars : t -> Act_common.C_id.Set.t
  (** [cvars program] extracts a set of C variable names from [program]. *)

  (** {3 Mutators} *)

  val with_functions : t -> Function.t id_assoc -> t
  (** [with_functions prog new_functions] creates a new program by
      substituting [new_functions] for [prog]'s functions. *)

  (** {3 Traversals} *)

  (** [On_decls] allows traversal over all of the declarations inside a
      program. *)
  module On_decls :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Initialiser.t named
end
