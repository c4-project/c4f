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

open Base
open Filter_chain_types

module Chain_output : sig
  type 'aux_o t = Checking_ahead | Skipped | Ran of 'aux_o
end

(** {2 Type synonyms} *)

module type Basic_unconditional =
  Basic_unconditional
  with type 'aux ctx := 'aux Filter_context.t
   and type 'aux chain_output := 'aux Chain_output.t

module type Basic_conditional_first =
  Basic_conditional_first
  with type 'aux_i ctx := 'aux_i Filter_context.t
   and type 'aux_o chain_output := 'aux_o Chain_output.t

module type Basic_conditional_second =
  Basic_conditional_second
  with type 'aux_i ctx := 'aux_i Filter_context.t
   and type 'aux_o chain_output := 'aux_o Chain_output.t

(** {2 Chaining filters together} *)

(** Chains two filters together using temporary files. *)
module Make (B : Basic_unconditional) :
  Filter.S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o * B.Second.aux_o

(** Simplified version of {{!Chain} Chain} that assumes that the auxiliary
    input is a tuple of the input to the first filter, and a function for
    generating the input to the second filter. *)
module Make_tuple (First : Filter.S) (Second : Filter.S) :
  Filter.S
  with type aux_i =
              First.aux_i * (First.aux_o Chain_output.t -> Second.aux_i)
   and type aux_o = First.aux_o * Second.aux_o

(** Chains an optional filter onto a mandatory one. *)
module Make_conditional_first (B : Basic_conditional_first) :
  Filter.S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o option * B.Second.aux_o

(** Chains a mandatory filter onto an optional one. *)
module Make_conditional_second (B : Basic_conditional_second) :
  Filter.S
  with type aux_i = B.aux_i
   and type aux_o = B.First.aux_o * B.Second.aux_o option
