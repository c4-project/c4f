open Core

module type Table =
  sig
    type t
    val table : (t, string) List.Assoc.t
  end

module type Intf =
  sig
    type t
    val of_string : string -> t option
    val of_string_exn : string -> t
    val to_string : ?equal:(t -> t -> bool) -> t -> string option
    val to_string_exn : ?equal:(t -> t -> bool) -> t -> string
  end

module Make (T : Table) =
  struct
    type t = T.t

    let rev_table =
      lazy
        (* TODO(@MattWindsor91): throwing exceptions here is scary. *)
        (Map.of_alist_exn (module String.Caseless) (List.Assoc.inverse T.table))

    let of_string str = Map.find (Lazy.force rev_table) str
    let of_string_exn str = Map.find_exn (Lazy.force rev_table) str
    let to_string ?(equal = (=)) t = List.Assoc.find ~equal:equal T.table t
    let to_string_exn ?(equal = (=)) t = List.Assoc.find_exn ~equal:equal T.table t
  end
