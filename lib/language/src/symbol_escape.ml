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
module Tx = Travesty_base_exts

let escape_string : string -> string =
  Staged.unstage
    ((* We could always just use something like Base36 here, but this seems
        a bit more human-readable. *)
     String.Escaping.escape_gen_exn
       ~escape_char:
         'Z'
         (* We escape some things that couldn't possibly appear in legal x86
            assembler, but _might_ be generated during sanitisation. *)
       ~escapeworthy_map:
         [ ('+', 'A') (* Add *)
         ; (',', 'C') (* Comma *)
         ; ('$', 'D') (* Dollar *)
         ; ('.', 'F') (* Full stop *)
         ; ('-', 'M') (* Minus *)
         ; ('%', 'P') (* Percent *)
         ; ('@', 'T') (* aT *)
         ; ('_', 'U') (* Underscore *)
         ; ('Z', 'Z')
           (* Z *) ])

module Make (S : Symbol_types.S) = struct
  let escape : S.t -> S.t = S.On_strings.map ~f:escape_string

  let escape_all : S.t list -> (S.t, S.t) List.Assoc.t =
    List.map ~f:(fun x -> (x, escape x))

  let escape_rmap (map : S.R_map.t) ~(to_escape : Set.M(S).t) : S.R_map.t =
    let escapes = escape_all (Set.to_list to_escape) in
    List.fold escapes ~init:map ~f:(fun map (src, dst) ->
        S.R_map.redirect map ~src ~dst)
end
