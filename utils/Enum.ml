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

(** Helper functions and modules for enums *)

open Core

module type S = sig
  type t

  val min : int
  val max : int
  val to_enum : t -> int
  val of_enum : int -> t option
end

module type SSexp = sig
  include S
  include Sexpable.S with type t := t
end

module type SSexpTable = sig
  include SSexp
  include StringTable.Table with type t := t
end

module type Extension = sig
  type t

  include Comparable.S with type t := t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t

  val to_enum : t -> int
  val of_enum : int -> t option
  val of_enum_exn : int -> t
  val min_enum : int
  val max_enum : int
  val all_list : unit -> t list
  val all_set : unit -> Set.t
end

module type ExtensionTable = sig
  include Extension
  include StringTable.Intf with type t := t
  include Identifiable.S_common with type t := t

  val pp_set : Format.formatter -> Set.t -> unit
end

module EnumCompare (E : SSexp)
  : Comparable.S with type t := E.t =
  Comparable.Make (struct
    include E

    let compare x y = Int.compare (E.to_enum x) (E.to_enum y);;
  end)

module EnumHash (E : SSexp)
  : Hashable.S with type t := E.t =
  Hashable.Make (struct
    include E
    include EnumCompare (E)
    let hash x = Int.hash (E.to_enum x);;
    let hash_fold_t s x = Int.hash_fold_t s (E.to_enum x);;
  end)

module Extend (E : SSexp)
  : Extension with type t := E.t = struct
  include EnumCompare (E)
  include EnumHash (E)

  include (E : Sexpable.S with type t := E.t)

  let of_enum = E.of_enum;;
  let to_enum = E.to_enum;;

  let of_enum_exn k = Option.value_exn (of_enum k);;

  let min_enum = E.min;;
  let max_enum = E.max;;

  let all_list () =
    List.map ~f:of_enum_exn
      (List.range ~stride:1 ~start:`inclusive ~stop:`inclusive E.min E.max)
  ;;

  let all_set () = Set.of_list (all_list ());;
end

module ExtendTable (E : SSexpTable) : ExtensionTable with type t := E.t = struct
  module Ex = Extend (E)
  include Ex

  module Tbl = StringTable.Make (E)
  include Tbl

  module Id : (Identifiable.S_common with type t := E.t) =
    StringTable.ToIdentifiable (Tbl)
      (struct
        let compare = Ex.compare
        let hash = Ex.hash
        let hash_fold_t = Ex.hash_fold_t
      end)
  include Id

  let pp_set f set =
    match Set.to_list set with
    | [] -> ()
    | xs ->
      Format.pp_print_space f ();
      Format.pp_print_char f '(';
      Format.pp_open_hovbox f 0;
      Format.pp_print_list ~pp_sep:MyFormat.pp_csep pp f xs;
      Format.pp_close_box f ();
      Format.pp_print_char f ')'
  ;;
end
