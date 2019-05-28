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

open Base

module Program = struct
  type ('warn_elt, 'listing) t =
    { warnings: 'warn_elt Warn.t list
    ; listing: 'listing
    ; symbol_table: Act_abstract.Symbol.Table.t }
  [@@deriving fields, make]

  let strip_listing (p : ('warn_elt, _) t) : ('warn_elt, unit) t =
    { p with listing = () }
end

type ('warn_elt, 'listing, 'rmap) t =
  { programs: ('warn_elt, 'listing) Program.t list;
    redirects: 'rmap
  }
[@@deriving fields, make]

let map_programs (output : ('w1, 'l1, 'rmap) t)
    ~(f: ('w1, 'l1) Program.t -> ('w2, 'l2) Program.t)
    : ('w2, 'l2, 'rmap) t =
  { output with programs = List.map ~f output.programs }

let map_redirects (output : ('w, 'l, 'r1) t)
    ~(f: 'r1 -> 'r2)
    : ('w, 'l, 'r2) t =
  { output with redirects = f output.redirects }
