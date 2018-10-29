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

include Core
include Utils
include Language_symbol_intf

module Make (B : Basic) = struct
  include B

  module Set = struct
    module M = Set.Make (B)
    include M
    include MyContainers.SetExtend (M)

    let abstract = Abstract.Symbol.Set.map ~f:B.abstract
  end

  module OnStrings = FoldMap.MakeSet (OnStringsS) (String.Set)

  let program_id_of sym =
    let asyms = B.abstract_demangle sym in
    let ids =
      List.filter_map asyms ~f:Abstract.Symbol.program_id_of
    in
    (* The demangled symbols should be in order of likelihood, so we
         take a gamble on the first one being the most likely program
         ID in case of a tie. *)
    List.hd ids
  ;;

  let is_program_label sym = Option.is_some (program_id_of sym)
end
