(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Tx = Travesty_base_exts
end

(* This module gymnastics brought to you by needing to have a comparator
   module below. *)
module M = struct
  module M_inner = struct
    type t = In_loop | In_dead_code | In_atomic [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [ (In_loop, "in loop")
      ; (In_dead_code, "in dead code")
      ; (In_atomic, "in atomic block") ]
  end

  include M_inner
  include Act_utils.Enum.Extend_table (M_inner)
end

include M

(** Maps a subset of the flags to predicates that toggle whether a piece of
    metadata sets the flag or not. *)
let metadata_predicates : (t, Metadata.t -> bool) List.Assoc.t =
  [(In_dead_code, Metadata.is_dead_code)]

let flags_of_flow_class : Act_fir.Statement_class.Flow.t -> Set.M(M).t =
  function
  | While _ ->
      Set.singleton (module M) In_loop
  | Lock (Some Atomic) ->
      Set.singleton (module M) In_atomic
  | Lock _ ->
      Set.empty (module M)

let flags_of_metadata (m : Metadata.t) : Set.M(M).t =
  metadata_predicates
  |> List.filter_map ~f:(fun (flag, f) -> Option.some_if (f m) flag)
  |> Set.of_list (module M)

let option_map (type a) (o : a Option.t) ~(f : a -> Set.M(M).t) : Set.M(M).t
    =
  Option.value_map o ~f ~default:(Set.empty (module M))

let flags_of_block (b : Subject.Block.t) : Set.M(M).t =
  flags_of_metadata (Act_fir.Block.metadata b)

let flags_of_stm (s : Subject.Statement.t) : Set.M(M).t =
  s |> Act_fir.Statement.own_metadata |> option_map ~f:flags_of_metadata

let flags_of_flow (f : Subject.Statement.Flow.t) : Set.M(M).t =
  f |> Act_fir.Statement_class.Flow.classify
  |> option_map ~f:flags_of_flow_class

module Flagged = struct
  type 'p t = {path: 'p; flags: Set.M(M).t} [@@deriving make, fields]

  include (
    Travesty.Bi_traversable.Make1_left (struct
      type nonrec 'p t = 'p t

      type right = Set.M(M).t

      module On_monad (Mo : Monad.S) = struct
        let bi_map_m (x : 'a t) ~(left : 'a -> 'b Mo.t)
            ~(right : right -> right Mo.t) : 'b t Mo.t =
          Mo.Let_syntax.(
            let%map path' = left x.path and flags' = right x.flags in
            {path= path'; flags= flags'})
      end
    end) :
      Travesty.Bi_traversable_types.S1_left
        with type 'p t := 'p t
         and type right = Set.M(M).t )
end
