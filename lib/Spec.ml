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

(** [Spec] contains general interfaces for dealing with specifications
    of machines and compilers. *)

open Core
open Utils

module Id = struct
  module T = struct
    (** [t] is the type of compiler IDs. *)
    type t = string list [@@deriving compare, hash, sexp, bin_io]

    let allowed_id_splits = [ '.' ; ' '; '/'; '\\']

    let of_string = String.split_on_chars ~on:allowed_id_splits

    let to_string = String.concat ~sep:"."

    let module_name = "act.Lib.Compiler.Id"
  end

  include T
  include Identifiable.Make (T)

  let to_string_list = Fn.id
end

(** [Basic] is the basic interface of both compiler and machine
   specifications. *)
module type Basic = sig
  (** [t] is the opaque type of specifications.
      To construct a [t], read one in as an S-expression;
      a proper constructor may appear in later revisions. *)
  type t [@@deriving sexp]

  (** [enabled c] gets whether [c] is enabled. *)
  val enabled : t -> bool

  include Pretty_printer.S with type t := t

  (** [pp_summary f spec] prints a one-line summary of [spec]. *)
  val pp_summary : Format.formatter -> t -> unit
end

module type S_with_id = sig
  type elt
  type t [@@deriving sexp]
  val create : id:Id.t -> spec:elt -> t
  val id : t -> Id.t
  val spec : t -> elt
  val to_tuple : t -> (Id.t * elt)
end

(** [S] is the top-level, outward-facing interface of both
   compiler and machine specifications. *)
module type S = sig
  include Basic

  (** [With_id] contains types and functions for handling bundles of
     spec ID and spec. *)
  module With_id : S_with_id with type elt = t

  (** [Set] is the interface of modules for dealing with sets of
      compiler specs. *)
  module Set : sig
    type elt = t

    (** [t] is the type of sets. *)
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    (** [pp_verbose verbose f specs] prints a [specs] with the level of
        verbosity implied by [verbose]. *)
    val pp_verbose : bool -> Format.formatter -> t -> unit

    (** [get specs id] tries to look up ID [id] in [specs],
        and emits an error if it can't. *)
    val get : t -> Id.t -> elt Or_error.t

    (** [of_list xs] tries to make a set from [xs].
        It raises an error if [xs] contains duplicate IDs. *)
    val of_list : With_id.t list -> t Or_error.t

    (** [partition_map specs ~f] applies a partitioning predicate [f] to the
        specifications in [specs], returning those marked [`Fst] in
        the first bucket and those marked [`Snd] in the second. *)
    val partition_map
      :  t
      -> f : (With_id.t -> [`Fst of 'a | `Snd of 'b])
      -> ('a list * 'b list)
    ;;

    (** [group specs ~f] groups [specs] into buckets according to some
       grouping function [f].  [f] returns specification IDs; the idea
       is that this allows grouping of specifications by references
        to other, larger specifications. *)
    val group
      :  t
      -> f : (With_id.t -> Id.t)
      -> t Id.Map.t
    ;;

    (** [map specs ~f] applies a mapper [f] to the
        specifications in [specs], returning the results as a list. *)
    val map
      :  t
      -> f : (With_id.t -> 'a)
      -> 'a list
    ;;
  end

  (** [pp_verbose verbose f spec] prints a [spec] with the level of
      verbosity implied by [verbose]. *)
  val pp_verbose : bool -> Format.formatter -> t -> unit
end

(** [Make] makes an [S] from a [Basic]. *)
module Make (B : Basic) : S with type t = B.t = struct
  include B

  module With_id = struct
    type elt = B.t

    type t =
      { id   : Id.t
      ; spec : B.t
      }
    [@@deriving fields, sexp]
    ;;

    let create = Fields.create

    let to_tuple {id; spec} = (id, spec)
  end

  let pp_verbose verbose = if verbose then pp else pp_summary

  module Set = struct
    type elt = B.t

    (* Wrapping this so that we can use [of_sexp] below. *)
    module SM = struct
      type t = (Id.t, B.t) List.Assoc.t [@@deriving sexp]
      let partition_map t ~f =
        List.partition_map t
          ~f:(fun (id, spec) -> f (With_id.create ~id ~spec))
      ;;

      let map t ~f =
        List.map t
          ~f:(fun (id, spec) -> f (With_id.create ~id ~spec))
      ;;
    end
    include SM

    let get specs id =
      List.Assoc.find specs ~equal:(Id.equal) id
      |> Result.of_option
        ~error:(Error.create_s [%message "unknown compiler ID" ~id:(id : Id.t)])
    ;;

    let of_list xs =
      let open Or_error.Let_syntax in
      let%map () =
        xs
        |> List.find_all_dups ~compare:(My_fn.on With_id.id Id.compare)
        |> List.map
          ~f:(fun x -> Or_error.error_s [%message "duplicate ID" ~id:(With_id.id x : Id.t)])
        |> Or_error.combine_errors_unit
      in
      List.map ~f:(fun x -> (With_id.id x, With_id.spec x)) xs
    ;;

    let group t ~f =
      t
      |> List.map
        ~f:(fun (id, spec) ->
            (f (With_id.create ~id ~spec), (id, spec)))
      |> Id.Map.of_alist_multi
    ;;

    let pp_id_spec f ~pp id spec =
      My_format.pp_kv f
        (Id.to_string id)
        pp spec
    ;;

    let pp_verbose verbose f specs =
      Format.pp_open_vbox f 0;
      Format.pp_print_list
        ~pp_sep:Format.pp_print_cut
        (fun f ->
           Tuple2.uncurry
           (pp_id_spec ~pp:(pp_verbose verbose) f))
        f
        specs;
      Format.pp_close_box f ();
    ;;

    let pp = pp_verbose true
  end
end
