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

open Core_kernel
open Act_utils
include Statement_intf
module Tx = Travesty_core_kernel_exts

module Make (B : Basic_with_modules) :
  S with type t = B.t and module Instruction = B.Instruction = struct
  include B
  module Symbol = Instruction.Symbol

  module Extended_flag = struct
    module M = struct
      type t = [Act_abstract.Statement.Flag.t | `Program_boundary]
      [@@deriving sexp, eq, enumerate]

      let table =
        (Act_abstract.Statement.Flag.table :> (t, string) List.Assoc.t)
        @ [(`Program_boundary, "program boundary")]
    end

    include M

    include Enum.Extend_table (struct
      include M
      include Enum.Make_from_enumerate (M)
    end)
  end

  include Act_abstract.Statement.Inherit_properties
            (Act_abstract.Statement)
            (struct
              type nonrec t = t

              let component = abstract
            end)

  let is_program_boundary stm =
    is_label stm && On_symbols.for_all stm ~f:Symbol.is_program_label

  let is_unused_ordinary_label stm ~symbol_table =
    (not (is_program_boundary stm)) && is_unused_label stm ~symbol_table

  let coerced_flags stm symbol_table =
    let abs_flags = flags stm symbol_table in
    let coerce x = (x :> Extended_flag.t) in
    Extended_flag.Set.map abs_flags ~f:coerce

  let new_flags stm _symbol_table =
    if is_program_boundary stm then
      Extended_flag.Set.singleton `Program_boundary
    else Extended_flag.Set.empty

  let extended_flags stm symbol_table =
    Extended_flag.Set.union
      (coerced_flags stm symbol_table)
      (new_flags stm symbol_table)

  let make_uniform : t list list -> t list list =
    Tx.List.right_pad ~padding:(empty ())
end
