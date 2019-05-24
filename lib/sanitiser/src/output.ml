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

include Output_intf

module Program = struct
  type ('warn_elt, 'listing) t =
    { warnings: 'warn_elt Warn.t list
    ; listing: 'listing
    ; symbol_table: Act_abstract.Symbol.Table.t }
  [@@deriving fields, make]
end

module Make (B : Basic) :
  S
  with type listing = B.listing
   and type warn_elt = B.warn_elt
   and type 'l pc = 'l B.pc
   and type rmap = B.rmap
   and type ('warn_elt, 'listing) program := ('warn_elt, 'listing) Program.t =
struct
  include B

  module Program = struct
    type t = (warn_elt, listing) Program.t

    let warnings = Program.warnings

    let symbol_table = Program.symbol_table

    let listing = Program.listing

    let make = Program.make
  end

  type t = {programs: Program.t pc; redirects: rmap}
  [@@deriving fields, make]
end
