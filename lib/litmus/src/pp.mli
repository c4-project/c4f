(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Pretty-printers for Litmus tests *)

module Generic : sig
  val pp_post :
       Base.Formatter.t
    -> 'const Postcondition.t
    -> pp_const:'const Fmt.t
    -> unit
  (** [pp_post f post ~pp_const] pretty_prints a postcondition, using the
      pretty-printer [pp_const] for constants. *)
end

(** {2 Functors for existing ASTs} *)

(* TODO(@MattWindsor91): reduce coupling of these to the AST/Lang modules *)

module Make_tabular (Ast : Ast.S) : Pp_intf.S with module Ast = Ast
(** [Make_tabular] makes a pretty-printer for a Litmus AST that outputs
    programs as tables.

    This is useful for assembly languages. *)

module Make_sequential (Ast : Ast.S) : Pp_intf.S with module Ast = Ast
(** [Make_sequential] makes a pretty-printer for a Litmus AST that directly
    outputs each program AST in sequence.

    This is useful for languages like C. *)
