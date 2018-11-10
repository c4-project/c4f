open Core

module type Table = sig
  type t
  val table : (t, string) List.Assoc.t
end

module type S = sig
  include Table
  val of_string : string -> t option
  val of_string_exn : string -> t
  val to_string : ?equal:(t -> t -> bool) -> t -> string option
  val to_string_exn : ?equal:(t -> t -> bool) -> t -> string
end

module Make (T : Table) = struct
  include T

  let rev_table =
    lazy
      (* TODO(@MattWindsor91): throwing exceptions here is scary. *)
      (Map.of_alist_exn (module String.Caseless) (List.Assoc.inverse T.table))

  let of_string str = Map.find (Lazy.force rev_table) str
  let of_string_exn str = Map.find_exn (Lazy.force rev_table) str
  let to_string ?(equal = (=)) t = List.Assoc.find ~equal:equal T.table t
  let to_string_exn ?(equal = (=)) t = List.Assoc.find_exn ~equal:equal T.table t
end

module type Basic_identifiable = sig
  type t
  include S with type t := t

  val compare : t -> t -> int
  val hash : t -> int
  val hash_fold_t : Hash.state -> t -> Hash.state
end

module To_identifiable (T : Basic_identifiable)
  : Identifiable.S_plain with type t := T.t =
  Identifiable.Make_plain (struct
    module M = struct
      include T

      module S = struct
        type nonrec t = t

        let of_string = T.of_string_exn;;
        (* There's a cyclic dependency between the comparable we want
           to build and the sexpable we want to build, so we can't use
           'equal' here *)
        let to_string = T.to_string_exn ~equal:(fun x y -> T.compare x y = 0);;
      end

      include Sexpable.Of_stringable (S)
      include (S : Stringable.S with type t := t)
    end

    include M

    (* Comparable *)
    module C = Comparable.Make_plain(M)
    include C

    (* Hashable *)
    include Hashable.Make_plain(M)

    let module_name = "act.Utils.String_table";;
  end
  )
