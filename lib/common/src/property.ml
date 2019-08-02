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

module Tree_doc = struct
  type elt = {args: string list; details: string}

  type t = (string, elt) List.Assoc.t

  let rearrange ((name, mdoc) : string * elt option) :
      (string * string list) * string =
    match mdoc with
    | Some {args; details} ->
        ((name, args), details)
    | None ->
        ((name, []), "DOC MISSING")

  let pp_header : (string * string list) Fmt.t =
    Fmt.(
      using
        (function k, [] -> (k, None) | k, v -> (k, Some v))
        (pair ~sep:nop string (option (sp ++ list ~sep:sp string))))

  let pp_maybe : (string * elt option) Fmt.t =
    Fmt.(
      vbox ~indent:2
        (using rearrange (pair ~sep:sp (hbox pp_header) (box words))))

  let name_to_doc (tree_docs : t) (name : string) : string * elt option =
    (name, List.Assoc.find tree_docs name ~equal:String.Caseless.equal)

  let pp (tree_docs : t) : string list -> unit Fmt.t =
    Fmt.(const (list ~sep:sp (using (name_to_doc tree_docs) pp_maybe)))
end
