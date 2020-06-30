(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Spec_types

open struct
  module Au = Act_utils
  module Tx = Travesty_base_exts
end

module With_id = struct
  type 'spec t = {id: Id.t; spec: 'spec} [@@deriving fields, make, equal]

  module T : Travesty.Traversable_types.S1 with type 'spec t := 'spec t =
  Travesty.Traversable.Make1 (struct
    type nonrec 'spec t = 'spec t

    module On_monad (M : Monad.S) = struct
      let map_m (type s1 s2) (wid : s1 t) ~(f : s1 -> s2 M.t) : s2 t M.t =
        let id = id wid in
        M.Let_syntax.(
          let%map spec = f (spec wid) in
          make ~id ~spec)
    end
  end)

  include T
end

module Set = struct
  type 'spec t = (Id.t * 'spec) list [@@deriving equal, sexp]

  let empty (type a) : a t = []

  let partition_map (type a b) (t : 'spec t)
      ~(f : 'spec With_id.t -> (a, b) Either.t) : a list * b list =
    List.partition_map t ~f:(fun (id, spec) -> f (With_id.make ~id ~spec))

  let map (type a) (t : 'spec t) ~(f : 'spec With_id.t -> a) : a list =
    List.map t ~f:(fun (id, spec) -> f (With_id.make ~id ~spec))

  let of_map (type spec) : spec Map.M(Id).t -> spec t = Map.to_alist

  let of_list (type a) (xs : a With_id.t list) : a t Or_error.t =
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

  let to_list (type a) (xs : a t) : a With_id.t list =
    List.map ~f:(fun (id, spec) -> With_id.make ~id ~spec) xs

  let get (type spec) ?(id_type : string = "unknown") (specs : spec t)
      ~(id : Id.t) : spec Or_error.t =
    Id.try_find_assoc_with_suggestions specs id ~id_type

  let get_prefix_fallback (type spec) ?(id_type : string option)
      (specs : spec t) (defaults : Id.t list) (initial_error : Error.t)
      ~(fqid : Id.t) : spec Or_error.t =
    let result =
      Or_error.find_map_ok defaults ~f:(fun default ->
          let fqid = Id.(default @. fqid) in
          get ?id_type specs ~id:fqid )
    in
    (* We want default resolution to be 'hidden' in the error case; errors
       returned should refer to the original resolution attempt. *)
    Travesty_base_exts.Or_error.map_right result ~f:(Fn.const initial_error)

  let get_with_fqid (type spec) ?(id_type : string option) (specs : spec t)
      ~(prefixes : Id.t list) ~(fqid : Id.t) =
    match (prefixes, get ?id_type specs ~id:fqid) with
    | [], result | _, (Ok _ as result) ->
        result
    | _, Error err ->
        get_prefix_fallback ?id_type specs prefixes err ~fqid

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

    let group t ~f =
      t
      |> List.map ~f:(fun (id, spec) ->
             (f (With_id.make ~id ~spec), (id, spec)) )
      |> Id.Map.of_alist_multi
  end
end

let pp_enabled_summary (f : Formatter.t) : bool -> unit =
  Fmt.(
    function
    | true ->
        styled (`Fg `Green) (any "enabled") f ()
    | false ->
        styled (`Fg `Red) (any "disabled") f ())
