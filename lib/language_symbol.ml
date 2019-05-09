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

include Core
include Utils
include Language_symbol_intf

let program_id_of_demangled sym =
  let open Option.Let_syntax in
  let%bind num_s = String.chop_prefix ~prefix:"P" sym in
  let%bind num = Caml.int_of_string_opt num_s in
  Option.some_if (Int.is_non_negative num) num

let%expect_test "program_id_of_demangled: valid" =
  Sexp.output Out_channel.stdout
    [%sexp (program_id_of_demangled "P0" : int option)] ;
  [%expect {| (0) |}]

module Make (B : Basic) : S with type t = B.t = struct
  include B

  let of_string_opt (s : string) = Result.ok (B.require_of_string s)

  module Comp = Comparable.Make (B)

  module Set = struct
    include My_set.Extend (Comp.Set)

    let abstract = Abstract.Symbol.Set.map ~f:B.abstract
  end

  include (
    Comp :
      Comparable.S
      with type t := t
       and type comparator_witness = Set.Elt.comparator_witness
       and module Set := Set )

  module R_map : Redirect_map.S with type sym = t and type sym_set = Set.t =
  Redirect_map.Make (struct
    include B
    include Comp

    let of_string (x : string) = Option.value_exn (of_string_opt x)

    let to_c_identifier (s : t) : C_identifier.t Or_error.t =
      s |> to_string |> C_identifier.create

    let of_c_identifier (id : C_identifier.t) : t Or_error.t =
      id |> C_identifier.to_string |> of_string_opt
      |> Result.of_option
           ~error:
             (Error.create_s
                [%message
                  "Couldn't convert identifier to symbol" ~here:[%here]
                    ~id:(id : C_identifier.t)])
  end)

  let program_id_of sym =
    let asyms = B.abstract_demangle sym in
    let ids = List.filter_map asyms ~f:program_id_of_demangled in
    (* The demangled symbols should be in order of likelihood, so we take a
       gamble on the first one being the most likely program ID in case of a
       tie. *)
    List.hd ids

  let is_c_safe (sym : t) : bool =
    Utils.C_identifier.is_string_safe (to_string sym)

  let is_herd_safe (sym : t) : bool =
    Utils.C_identifier.Herd_safe.is_string_safe (to_string sym)

  let is_program_label sym = Option.is_some (program_id_of sym)
end

module String_direct : S with type t = string = Make (struct
  include String

  let abstract = Fn.id

  let abstract_demangle = List.return

  let require_of_string = Or_error.return

  module On_strings =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (String)
end)
