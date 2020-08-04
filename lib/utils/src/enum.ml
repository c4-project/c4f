(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
module Tx = Travesty_base_exts

module Make_comparable (E : Enum_types.S_sexp) :
  Comparable.S with type t := E.t = Comparable.Make (struct
  include E

  let compare = Comparable.lift ~f:E.to_enum Int.compare
end)

module Make_hashable (E : Enum_types.S) : sig
  type t = E.t [@@deriving hash]
end = struct
  type t = E.t

  let hash x = Int.hash (E.to_enum x)

  let hash_fold_t s x = Int.hash_fold_t s (E.to_enum x)
end

module Make_quickcheck (E : sig
  type t [@@deriving hash]

  include Enum_types.S_sexp with type t := t
end) : My_quickcheck.S_with_sexp with type t = E.t = struct
  type t = E.t

  let sexp_of_t = E.sexp_of_t

  module G = Base_quickcheck.Generator
  module O = Base_quickcheck.Observer
  module S = Base_quickcheck.Shrinker

  let of_enum_exn x = x |> E.of_enum |> Option.value_exn

  let quickcheck_generator =
    G.map ~f:of_enum_exn (G.int_uniform_inclusive E.min E.max)

  let quickcheck_observer = O.of_hash_fold E.hash_fold_t

  let quickcheck_shrinker = S.atomic
end

module Make_from_enumerate (E : Enum_types.S_enumerate) :
  Enum_types.S with type t = E.t = struct
  type t = E.t

  let min = 0

  let max = List.length E.all - 1

  let to_enum elt =
    List.find_mapi_exn E.all ~f:(fun i elt' ->
        Option.some_if (E.equal elt elt') i)

  let of_enum = List.nth E.all
end

module Extend (E : Enum_types.S_sexp) :
  Enum_types.Extension with type t := E.t = struct
  module EH = struct
    include E
    include Make_hashable (E)
  end

  module EC = struct
    include EH
    include Make_quickcheck (EH)
    include Make_comparable (EH)
  end

  include EC

  let of_enum = E.of_enum

  let to_enum = E.to_enum

  let of_enum_exn k = Option.value_exn (of_enum k)

  let min_enum = E.min

  let max_enum = E.max

  let all_list () =
    List.map ~f:of_enum_exn
      (List.range ~stride:1 ~start:`inclusive ~stop:`inclusive E.min E.max)

  let all_set () = Set.of_list (module EC) (all_list ())
end

module Extend_table (E : Enum_types.S_table) :
  Enum_types.Extension_table with type t := E.t = struct
  module Tbl = String_table.Make (struct
    include E

    let equal = Comparable.lift Int.equal ~f:E.to_enum
  end)

  include Tbl

  module Basic_id = struct
    include E
    include Make_hashable (E)
    include Tbl
  end

  module Id : Stringable.S with type t = E.t = struct
    type t = E.t

    include String_table.To_stringable (Basic_id)
  end

  include Id
  include Plumbing.Jsonable.Of_stringable (Id)
  module Sexp = Sexpable.Of_stringable (Id)

  include Extend (struct
    include E
    include Sexp
  end)

  let of_string_option = Tbl.of_string

  let pp : t Fmt.t = Fmt.of_to_string to_string
end
