(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Spec] contains general interfaces for dealing with specifications of
    machines and compilers. *)

open Core_kernel (* for Tuple2 *)

open Spec_types
module Au = Act_utils
module Tx = Travesty_core_kernel_exts

module Set = struct
  type 'spec t = (Id.t * 'spec) list [@@deriving equal]

  let empty (type a) : a t = []

  let restrict (set : 'spec t) ~(identifiers : Id.Set.t) : 'spec t =
    List.filter set ~f:(Fn.compose (Id.Set.mem identifiers) fst)

  let partition_map (type a b) (t : 'spec t)
      ~(f : Id.t -> 'spec -> [`Fst of a | `Snd of b]) : a list * b list =
    List.partition_map t ~f:(Tuple2.uncurry f)

  let map (type a) (t : 'spec t) ~(f : Id.t -> 'spec -> a) : a list =
    List.map t ~f:(Tuple2.uncurry f)

  let of_map (type spec) : spec Map.M(Id).t -> spec t = Map.to_alist

  let try_match_fqid (fqid : Id.t) (spec_id : Id.t) (spec : 'spec) :
      (Id.t * 'spec) option =
    Option.some_if (Id.is_prefix fqid ~prefix:spec_id) (spec_id, spec)

  let get_using_fqid (specs : 'spec t) ?(id_type : string = "unknown")
      ~(fqid : Id.t) : (Id.t * 'spec) Or_error.t =
    specs
    |> List.find_map ~f:(Tuple2.uncurry (try_match_fqid fqid))
    |> Result.of_option
         ~error:
           (Error.create_s
              [%message
                "No specification exists matching all or part of this FQID."
                  ~id_type
                  ~fqid:(fqid : Id.t)])

  let get specs ?(id_type : string = "unknown") (id : Id.t) =
    Id.try_find_assoc_with_suggestions specs id ~id_type

  module On_specs : Travesty.Traversable.S1 with type 'a t = 'a t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'a t = 'a t

    module On_monad (M : Monad.S) = struct
      module LM = Tx.List.On_monad (M)

      let map_m (set : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        LM.map_m set ~f:(fun (i, v) -> M.(v |> f >>| Tuple2.create i))
    end
  end)
end

module type S = sig
  type t

  include Spec_types.S with type Set.t = t Set.t and type t := t
end

module With_id (C : Spec_types.Common) :
  Spec_types.S_with_id with type elt = C.t = struct
  type elt = C.t

  type t = {id: Id.t; spec: C.t} [@@deriving fields, make, equal]

  let is_enabled x = C.is_enabled (spec x)

  let pp_summary f x = C.pp_summary f (spec x)

  let pp f x = C.pp f (spec x)
end

module Make (B : Basic) :
  S with type t = B.t and module With_id = B.With_id = struct
  include B

  let pp_verbose verbose = if verbose then pp else pp_summary

  module Set = struct
    type t = B.t Set.t [@@deriving equal]

    let restrict : t -> identifiers:Id.Set.t -> t = Set.restrict

    let partition_map (t : t)
        ~(f : B.With_id.t -> [`Fst of 'a | `Snd of 'b]) : 'a list * 'b list
        =
      Set.partition_map t ~f:(fun id spec -> f (With_id.make ~id ~spec))

    let map (t : t) ~(f : B.With_id.t -> 'a) : 'a list =
      Set.map t ~f:(fun id spec -> f (With_id.make ~id ~spec))

    let get_using_fqid (specs : t) ~(fqid : Id.t) : With_id.t Or_error.t =
      Or_error.Let_syntax.(
        let%map id, spec =
          Set.get_using_fqid specs ~fqid ~id_type:type_name
        in
        With_id.make ~id ~spec)

    let get specs (id : Id.t) =
      Or_error.Let_syntax.(
        let%map spec = Set.get specs id ~id_type:type_name in
        With_id.make ~id ~spec)

    let of_list xs =
      let open Or_error.Let_syntax in
      let%map () =
        xs
        |> List.find_all_dups ~compare:(Tx.Fn.on With_id.id ~f:Id.compare)
        |> List.map ~f:(fun x ->
               Or_error.error_s
                 [%message "duplicate ID" ~id:(With_id.id x : Id.t)] )
        |> Or_error.combine_errors_unit
      in
      List.map ~f:(fun x -> (With_id.id x, With_id.spec x)) xs

    let group t ~f =
      t
      |> List.map ~f:(fun (id, spec) ->
             (f (With_id.make ~id ~spec), (id, spec)) )
      |> Id.Map.of_alist_multi

    let pp_id_spec f ~pp id spec =
      Au.My_format.pp_kv f (Id.to_string id) pp spec

    let pp_verbose verbose : t Fmt.t =
      Fmt.(
        vbox
          (list ~sep:cut (fun f ->
               Tuple2.uncurry (pp_id_spec ~pp:(pp_verbose verbose) f) )))

    let pp : t Fmt.t = pp_verbose true
  end
end
