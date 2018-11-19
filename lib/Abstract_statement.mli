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

open Utils

(** [t] is an abstracted statement. *)
type t =
  | Directive of string
  | Instruction of Abstract_instruction.t
  | Blank
  | Label of string
  | Other
;;

(** [Flag] is an enumeration of various statement observations. *)
module Flag : sig
  type t =
    [ `UnusedLabel   (* A label that doesn't appear in any jumps *)
    | `ProgBoundary  (* A label that looks like a program boundary *)
    | `StackManip    (* A statement that only serves to manipulate
                        the call stack *)
    ]

  (** [Flag] contains various enum extensions, including a [Set]
      type. *)
  include Enum.Extension_table with type t := t
end

include Abstract_base.S with type t := t and module Flag := Flag
