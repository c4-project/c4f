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
module Ac = Act_common

let program_id_of_demangled (sym : string) : int option =
  let open Option.Let_syntax in
  let%bind num_s = String.chop_prefix ~prefix:"P" sym in
  let%bind num = Caml.int_of_string_opt num_s in
  Option.some_if (Int.is_non_negative num) num

module Make (B : Symbol_types.Basic) : Symbol_types.S with type t = B.t =
struct
  module Sym = struct
    include B

    let of_string_opt (s : string) = Result.ok (B.require_of_string s)

    module Comp : Comparable.S with type t := t = Comparable.Make (B)

    include Comp
  end

  include Sym

  module R_map :
    Ac.Redirect_map_intf.S
      with type Sym.t = t
       and type Sym.comparator_witness = comparator_witness =
  Ac.Redirect_map.Make (struct
    include Sym

    let of_string (x : string) = Option.value_exn (of_string_opt x)

    let to_c_identifier (s : t) : Ac.C_id.t Or_error.t =
      s |> to_string |> Ac.C_id.create

    let of_c_identifier (id : Ac.C_id.t) : t Or_error.t =
      id |> Ac.C_id.to_string |> of_string_opt
      |> Result.of_option
           ~error:
             (Error.create_s
                [%message
                  "Couldn't convert identifier to symbol" ~here:[%here]
                    ~id:(id : Ac.C_id.t)])
  end)

  let program_id_of sym =
    let asyms = B.abstract_demangle sym in
    let ids = List.filter_map asyms ~f:program_id_of_demangled in
    (* The demangled symbols should be in order of likelihood, so we take a
       gamble on the first one being the most likely program ID in case of a
       tie. *)
    List.hd ids

  let is_c_safe (sym : t) : bool = Ac.C_id.is_string_safe (to_string sym)

  let is_herd_safe (sym : t) : bool =
    Ac.C_id.Herd_safe.is_string_safe (to_string sym)

  let is_program_label sym = Option.is_some (program_id_of sym)
end

module String_direct : Symbol_types.S with type t = string = Make (struct
  include String

  let abstract = Fn.id

  let abstract_demangle = List.return

  let require_of_string = Or_error.return

  module On_strings =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (String)
end)
