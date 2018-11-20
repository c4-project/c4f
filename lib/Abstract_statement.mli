(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** [t] is an abstracted statement. *)
type t =
  | Directive of string
  | Instruction of Abstract_instruction.with_operands
  | Blank
  | Label of string
  | Other
;;

(** [Flag] is an enumeration of various statement observations. *)
module Flag : sig
  type t =
    [ `UnusedLabel   (* A label that doesn't appear in any jumps *)
    | `StackManip    (* A statement that only serves to manipulate
                        the call stack *)
    ]
  [@@deriving sexp, enumerate]
  ;;

  include Abstract_flag.S with type t := t
end

(** [S_properties] is the signature of any module that can access
    properties of an abstract statement. *)
module type S_properties = sig
  (** [t] is the type we're querying. *)
  type t

  (** We can run some instruction-level queries on an abstract
      statement; they return [false] when the statement isn't an
      instruction. *)
  include Abstract_instruction.S_properties with type t := t

  (** [is_directive stm] decides whether [stm] appears to be an
      assembler directive. *)
  val is_directive : t -> bool

  (** [is_instruction stm] tests whether [stm] is an instruction. *)
  val is_instruction : t -> bool

  (** [is_instruction_where stm ~f] tests whether [stm] is a label
      whose opcode and operands satisfy the predicate [f]. *)
  val is_instruction_where
    :  t
    -> f:(Abstract_instruction.with_operands -> bool)
    -> bool
  ;;

  (** [is_label stm] decides whether [stm] appears to be an
      label. *)
  val is_label : t -> bool

  (** [is_label_where stm ~f] tests whether [stm] is a label
      whose symbol satisfies the predicate [f]. *)
  val is_label_where : t -> f:(string -> bool) -> bool

  (** [is_unused_label stm ~symbol_table] decides
     whether [stm] is a label whose symbol isn't registered as a jump
      destination in [symbol_table]. *)
  val is_unused_label
    : t
    -> symbol_table:Abstract_symbol.Table.t
    -> bool
  ;;

  (** [is_jump_pair x y] returns true if [x] is a jump instruction,
      [y] is a label, and [x] is jumping to [y]. *)
  val is_jump_pair : t -> t -> bool

  (** [is_blank stm] tests whether [stm] is a blank statement. *)
  val is_blank : t -> bool

  (** [flags x symbol_table] gets the statement flags for [x] given
      symbol table [symbol_table]. *)
  val flags : t -> Abstract_symbol.Table.t -> Flag.Set.t
end

(** [Forward_properties F] generates a [S_properties] for a type [fwd]
    given an [S_properties] for a type [t] and a function to project
    [fwd] to [t]. *)
module Forward_properties
  : functor (F : sig
               include S_properties
               type fwd
               val forward : fwd -> t
             end)
    -> S_properties with type t := F.fwd
;;

include S_properties with type t := t

include Abstract_base.S with type t := t and module Flag := Flag
