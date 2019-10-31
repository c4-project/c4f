(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Spec_types
module Au = Act_utils
module Tx = Travesty_base_exts

module With_id = struct
  type 'spec t = {id: Id.t; spec: 'spec} [@@deriving fields, make, equal]
end

module Set = struct
  type 'spec t = (Id.t * 'spec) list [@@deriving equal]

  let empty (type a) : a t = []

  let partition_map (type a b) (t : 'spec t)
      ~(f : 'spec With_id.t -> [`Fst of a | `Snd of b]) : a list * b list =
    List.partition_map t ~f:(fun (id, spec) -> f (With_id.make ~id ~spec))

  let map (type a) (t : 'spec t) ~(f : Id.t -> 'spec -> a) : a list =
    List.map t ~f:(fun (i, s) -> f i s)

  let of_map (type spec) : spec Map.M(Id).t -> spec t = Map.to_alist

  let of_list (type a) (xs : a With_id.t list) : a t Or_error.t =
    let open Or_error.Let_syntax in
    let%map () =
      xs
      |> List.find_all_dups ~compare:(Tx.Fn.on With_id.id ~f:Id.compare)
      |> List.map ~f:(fun x ->
             Or_error.error_s
               [%message "duplicate ID" ~id:(With_id.id x : Id.t)])
      |> Or_error.combine_errors_unit
    in
    List.map ~f:(fun x -> (With_id.id x, With_id.spec x)) xs

  let get_using_fqid (type spec) ?(id_type : string = "unknown")
      (specs : spec t) ~(fqid : Id.t) : (Id.t * spec) Or_error.t =
    Id.try_find_assoc_with_suggestions_prefix specs fqid ~id_type

  let get (type spec) ?(id_type : string = "unknown") (specs : spec t)
      ~(id : Id.t) : spec Or_error.t =
    Id.try_find_assoc_with_suggestions specs id ~id_type

  module On_specs : Travesty.Traversable_types.S1 with type 'a t = 'a t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'a t = 'a t

    module On_monad (M : Monad.S) = struct
      module LM = Tx.List.On_monad (M)

      let map_m (set : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
        LM.map_m set ~f:(fun (i, v) -> M.(v |> f >>| fun v' -> (i, v')))
    end
  end)
end

module type S = sig
  type t

  include Spec_types.S with type Set.t = t Set.t and type t := t
end

module Make_with_id (C : Spec_types.Common) :
  Spec_types.S_with_id with type elt = C.t and type t = C.t With_id.t =
struct
  type elt = C.t

  type t = C.t With_id.t [@@deriving equal]

  let id = With_id.id

  let spec = With_id.spec

  let make = With_id.make

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
        let%map spec = Set.get specs ~id ~id_type:type_name in
        With_id.make ~id ~spec)

    let group t ~f =
      t
      |> List.map ~f:(fun (id, spec) ->
             (f (With_id.make ~id ~spec), (id, spec)))
      |> Id.Map.of_alist_multi

    let pp_id_spec f ~pp id spec =
      Au.My_format.pp_kv f (Id.to_string id) pp spec

    let pp_verbose verbose : t Fmt.t =
      Fmt.(
        vbox
          (list ~sep:cut (fun f (i, s) ->
               (pp_id_spec ~pp:(pp_verbose verbose) f) i s)))

    let pp : t Fmt.t = pp_verbose true
  end
end
