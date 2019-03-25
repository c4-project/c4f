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

module type Common = sig
  type t [@@deriving sexp]

  val is_enabled : t -> bool

  include Pretty_printer.S with type t := t

  val pp_summary : Format.formatter -> t -> unit
end

module type S_with_id = sig
  type elt
  type t

  include Common with type t := t

  val create : id:Id.t -> spec:elt -> t
  val id : t -> Id.t
  val spec : t -> elt
end

module With_id (C : Common) : S_with_id with type elt := C.t = struct
  type t =
    { id : Id.t
    ; spec : C.t
    }
  [@@deriving fields, sexp]

  let create = Fields.create
  let is_enabled x = C.is_enabled (spec x)
  let pp_summary f x = C.pp_summary f (spec x)
  let pp f x = C.pp f (spec x)
end

module type Basic = sig
  type t

  include Common with type t := t
  module With_id : S_with_id with type elt := t
end

module type S = sig
  include Basic

  module Set : sig
    type t [@@deriving sexp]

    include Pretty_printer.S with type t := t

    val pp_verbose : bool -> Format.formatter -> t -> unit
    val get : t -> Id.t -> With_id.t Or_error.t
    val of_list : With_id.t list -> t Or_error.t
    val restrict : t -> Id.Set.t -> t

    val partition_map
      :  t
      -> f:(With_id.t -> [ `Fst of 'a | `Snd of 'b ])
      -> 'a list * 'b list

    val group : t -> f:(With_id.t -> Id.t) -> t Id.Map.t
    val map : t -> f:(With_id.t -> 'a) -> 'a list
  end

  val pp_verbose : bool -> Format.formatter -> t -> unit
end

module Make (B : Basic) : S with type t := B.t and module With_id := B.With_id = struct
  include B

  let pp_verbose verbose = if verbose then pp else pp_summary

  module Set = struct
    (* Wrapping this so that we can use [of_sexp] below. *)
    module SM = struct
      type t = (Id.t, B.t) List.Assoc.t [@@deriving sexp]

      let restrict (set : t) (identifiers : Id.Set.t) : t =
        List.filter set ~f:(Fn.compose (Id.Set.mem identifiers) fst)
      ;;

      let partition_map t ~f =
        List.partition_map t ~f:(fun (id, spec) -> f (With_id.create ~id ~spec))
      ;;

      let map t ~f = List.map t ~f:(fun (id, spec) -> f (With_id.create ~id ~spec))
    end

    include SM

    let get specs id =
      List.Assoc.find specs ~equal:Id.equal id
      |> Option.map ~f:(fun spec -> With_id.create ~id ~spec)
      |> Result.of_option
           ~error:(Error.create_s [%message "unknown compiler ID" ~id:(id : Id.t)])
    ;;

    let of_list xs =
      let open Or_error.Let_syntax in
      let%map () =
        xs
        |> List.find_all_dups ~compare:(Travesty.T_fn.on With_id.id Id.compare)
        |> List.map ~f:(fun x ->
               Or_error.error_s [%message "duplicate ID" ~id:(With_id.id x : Id.t)])
        |> Or_error.combine_errors_unit
      in
      List.map ~f:(fun x -> With_id.id x, With_id.spec x) xs
    ;;

    let group t ~f =
      t
      |> List.map ~f:(fun (id, spec) -> f (With_id.create ~id ~spec), (id, spec))
      |> Id.Map.of_alist_multi
    ;;

    let pp_id_spec f ~pp id spec = My_format.pp_kv f (Id.to_string id) pp spec

    let pp_verbose verbose f specs =
      Format.pp_open_vbox f 0;
      Format.pp_print_list
        ~pp_sep:Format.pp_print_cut
        (fun f -> Tuple2.uncurry (pp_id_spec ~pp:(pp_verbose verbose) f))
        f
        specs;
      Format.pp_close_box f ()
    ;;

    let pp = pp_verbose true
  end
end
