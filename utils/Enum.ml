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

module Make_compare_hash_basic (E : S) = struct
  let compare = My_fn.on E.to_enum Int.compare
  let hash x = Int.hash (E.to_enum x)
  let hash_fold_t s x = Int.hash_fold_t s (E.to_enum x)
end

module Make_comparable (E : S_sexp)
  : Comparable.S with type t := E.t =
  Comparable.Make (struct
    include E
    include Make_compare_hash_basic (E)
  end)
;;

module Make_hashable (E : S_sexp)
  : Hashable.S with type t := E.t =
  Hashable.Make (struct
    include E
    include Make_comparable (E)
    include Make_compare_hash_basic (E)
  end)
;;

module Make_from_enumerate (E : S_enumerate)
  : S with type t := E.t = struct
  let min = 0
  let max = (List.length E.all - 1)

  let to_enum elt =
    List.find_mapi_exn E.all
      ~f:(fun i elt' -> Option.some_if (E.equal elt elt') i)
  ;;
  let of_enum = List.nth E.all
end

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

module Extend_table (E : S_table)
  : Extension_table with type t := E.t = struct
  module Tbl = String_table.Make (E)
  include Tbl

  module Basic_id = struct
    type t = E.t
    include Tbl
    include Make_compare_hash_basic (E)
  end

  module Id : (Identifiable.S_common with type t := E.t) =
    String_table.To_identifiable (Basic_id)
  include Id

  (* Identifiable, for some reason, doesn't contain both sides
     of sexp conversion, so we have to manually retrieve
     [t_of_sexp] ourselves. *)
  module Sexp = struct
    module Sexp = Sexpable.Of_stringable (struct
        type t = E.t
        include String_table.To_stringable (Basic_id)
      end)
    let sexp_of_t = Id.sexp_of_t
    let t_of_sexp = Sexp.t_of_sexp
  end

  include Extend (struct
      include E
      include Sexp
    end)

  let of_string_option = Tbl.of_string

  let pp_set = Fmt.(using Set.to_list (parens (box (list ~sep:comma pp))))
end
