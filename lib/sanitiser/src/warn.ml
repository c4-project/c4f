(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base
open Warn_intf

type 'elt t = {program_name: string; element: 'elt; body: Info.t}
[@@deriving fields, make]

module Make (Elt : Act_language.Element_types.S) :
  S with type t = Elt.t t and type elt = Elt.t = struct
  type elt = Elt.t

  type nonrec t = elt t

  let make = make

  let program_name = program_name

  let element = element

  let body = body

  let pp f ent =
    Fmt.pf f "@[<v>@[<h>In program %s,@ in %s@ `%a`:@]@ %a@]"
      ent.program_name
      (Elt.type_name ent.element)
      Elt.pp ent.element Info.pp ent.body

  let not_understood () = Info.of_string "act didn't understand this element"

  let erroneous reason =
    Info.create_s
      [%message
        "act thinks this element is invalid" ~reason:(reason : Error.t)]
end
