open Base

module type Table = sig
  type t

  val table : (t, string) List.Assoc.t
end

module type S = sig
  include Table

  val of_string : string -> t option

  val of_string_exn : string -> t

  val to_string : t -> string option

  val to_string_exn : t -> string
end

module Make (T : sig
  type t [@@deriving equal]

  include Table with type t := t
end) : S with type t = T.t = struct
  include T

  let rev_table =
    lazy
      (* TODO(@MattWindsor91): throwing exceptions here is scary. *)
      (Map.of_alist_exn
         (module String.Caseless)
         (List.Assoc.inverse T.table))

  let of_string str = Map.find (Lazy.force rev_table) str

  let of_string_exn str = Map.find_exn (Lazy.force rev_table) str

  let to_string t = List.Assoc.find ~equal T.table t

  let to_string_exn t = List.Assoc.find_exn ~equal T.table t
end

module To_stringable (T : S) : Stringable.S with type t = T.t = struct
  type t = T.t

  let of_string = T.of_string_exn

  let to_string = T.to_string_exn
end
