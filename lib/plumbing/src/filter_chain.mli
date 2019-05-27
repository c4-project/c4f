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

(** Functors for chaining together {{!Filter} filters}. *)

open Filter_chain_types

(** {2 Chaining filters together} *)

(** Chains two filters together using temporary files. *)
module Make (B : Basic_unconditional) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o

(** Chains an optional filter onto a mandatory one. *)
module Make_conditional_first (B : Basic_conditional_first) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o

(** Chains a mandatory filter onto an optional one. *)
module Make_conditional_second (B : Basic_conditional_second) :
  Filter_types.S with type aux_i = B.aux_i and type aux_o = B.aux_o
