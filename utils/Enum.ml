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

open Core_kernel
include Enum_intf

module Make_comparable (E : S_sexp)
  : Comparable.S with type t := E.t =
  Comparable.Make (struct
    include E

    let compare = My_fn.on E.to_enum Int.compare
  end)
;;

module Make_hashable (E : S_sexp)
  : Hashable.S with type t := E.t =
  Hashable.Make (struct
    include E
    include Make_comparable (E)
    let hash x = Int.hash (E.to_enum x)
    let hash_fold_t s x = Int.hash_fold_t s (E.to_enum x)
  end)
;;

module Extend (E : S_sexp) : Extension with type t := E.t = struct
  include Make_comparable (E)
  include Make_hashable (E)

  include (E : Sexpable.S with type t := E.t)

  let of_enum = E.of_enum
  let to_enum = E.to_enum

  let of_enum_exn k = Option.value_exn (of_enum k);;

  let min_enum = E.min;;
  let max_enum = E.max;;

  let all_list () =
    List.map ~f:of_enum_exn
      (List.range ~stride:1 ~start:`inclusive ~stop:`inclusive E.min E.max)
  ;;

  let all_set () = Set.of_list (all_list ());;
end

module Extend_table (E : S_sexp_table)
  : Extension_table with type t := E.t = struct
  module Ex = Extend (E)
  include Ex

  module Tbl = String_table.Make (E)
  include Tbl

  module Id : (Identifiable.S_common with type t := E.t) =
    String_table.To_identifiable (struct
      type t = E.t
      include Tbl
      let compare = Ex.compare
      let hash = Ex.hash
      let hash_fold_t = Ex.hash_fold_t
    end)
  include Id

  let of_string_option = Tbl.of_string;;

  let pp_set f set =
    match Set.to_list set with
    | [] -> ()
    | xs ->
      Format.pp_print_space f ();
      Format.pp_print_char f '(';
      Format.pp_open_hovbox f 0;
      Format.pp_print_list ~pp_sep:My_format.pp_csep pp f xs;
      Format.pp_close_box f ();
      Format.pp_print_char f ')'
  ;;
end
