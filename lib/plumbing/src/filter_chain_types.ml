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

(** Signatures used in the plumbing module. *)

(** Basic signature of inputs needed to build a chain. *)
module type Basic = sig
  (** Combined auxiliary input. *)
  type aux_i

  (** The first filter. *)
  module First : Filter_types.S

  (** The second filter. *)
  module Second : Filter_types.S
end

(** Signature of inputs needed to build an unconditional chain. *)
module type Basic_unconditional = sig
  include Basic

  val first_input : aux_i -> First.aux_i
  (** [first_input in] should extract the input for the first chained filter
      from [in]. *)

  val second_input : aux_i -> First.aux_o Chain_context.t -> Second.aux_i
  (** [second_input in first_out] should extract the input for the second
      chained filter from [in] and the output [first_out] from the first
      filter. [first_out] may be missing; this usually occurs when the
      second input is needed before the first filter has run. *)
end

(** Signature of inputs needed to build a conditional chain. *)
module type Basic_conditional = sig
  include Basic

  (** Auxiliary input used when not chaining. *)
  type aux_i_single

  val select :
       aux_i Filter_context.t
    -> [ `Both of First.aux_i * (First.aux_o Chain_context.t -> Second.aux_i)
       | `One of aux_i_single ]
  (** [select ctx] should return [`Both] when the optional filter should be
      run (which filter this is depends on the functor). *)
end

(** Signature of inputs needed to build a conditional chain with the first
    filter being conditional. *)
module type Basic_conditional_first = sig
  (** The first filter. *)
  module First : Filter_types.S

  (** The second filter. *)
  module Second : Filter_types.S

  include
    Basic_conditional
    with module First := First
     and module Second := Second
     and type aux_i_single := First.aux_o Chain_context.t -> Second.aux_i
end

(** Signature of inputs needed to build a conditional chain with the second
    filter being conditional. *)
module type Basic_conditional_second = sig
  (** The first filter. *)
  module First : Filter_types.S

  include
    Basic_conditional
    with module First := First
     and type aux_i_single := First.aux_i
end
