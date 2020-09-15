(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* This module gymnastics brought to you by needing to have a comparator
   module below. *)
module M = struct
  module M_inner = struct
    type t =
      | Execute_multi_unsafe
      | In_atomic
      | In_dead_code
      | In_execute_multi
      | In_loop
    [@@deriving enum]

    let table : (t, string) List.Assoc.t =
      [ (Execute_multi_unsafe, "execute-multi-unsafe")
      ; (In_atomic, "in-atomic")
      ; (In_dead_code, "in-dead-code")
      ; (In_execute_multi, "in-execute-multi")
      ; (In_loop, "in-loop") ]
  end

  include M_inner
  include Act_utils.Enum.Extend_table (M_inner)
end

include M

(** Maps a subset of the flags to predicates that toggle whether a piece of
    metadata sets the flag or not. *)
let metadata_predicates : (t, Metadata.t -> bool) List.Assoc.t =
  [ ( Execute_multi_unsafe
    , Metadata.has_restriction Metadata.Restriction.Once_only )
  ; (In_dead_code, Metadata.(Fn.compose Liveness.is_dead liveness))
    (* We don't add Execute_multi here, as, while it depends on the block
       metadata, it only gets added to loops; consequently, it gets added as
       a special case in the flow flag generator. *) ]

let flags_of_flow_class' =
  [%accessor
    Accessor.optional_getter (function
      | Fir.Statement_class.Flow.For | While _ ->
          (* Whether or not the loop executes multiple times is stored in its
             block metadata. *)
          Some In_loop
      | Lock (Some Atomic) ->
          Some In_atomic
      | Lock _ | Explicit | Implicit ->
          None)]

let flags_of_metadata (m : Metadata.t) : Set.M(M).t =
  metadata_predicates
  |> List.filter_map ~f:(fun (flag, f) -> Option.some_if (f m) flag)
  |> Set.of_list (module M)

let option_map (type a) (o : a Option.t) ~(f : a -> Set.M(M).t) : Set.M(M).t
    =
  Option.value_map o ~f ~default:(Set.empty (module M))

let flags_of_block (b : Subject.Block.t) : Set.M(M).t =
  flags_of_metadata b.@(Fir.Block.metadata)

let flags_of_stm (s : Subject.Statement.t) : Set.M(M).t =
  s |> Fir.Statement.own_metadata |> option_map ~f:flags_of_metadata

let add_special_flow_flags (f : Subject.Statement.Flow.t) (fcf : Set.M(M).t)
    : Set.M(M).t =
  let body_m = (Fir.Flow_block.body f).@(Fir.Block.metadata) in
  if Set.mem fcf In_loop && Metadata.(Liveness.equal Live (liveness body_m))
  then Set.add fcf In_execute_multi
  else fcf

let classify' =
  [%accessor Accessor.optional_getter Fir.Statement_class.Flow.classify]

let flags_of_flow_class : Subject.Statement.Flow.t -> Set.M(M).t =
  Accessor.Set.of_accessor (module M) (classify' @> flags_of_flow_class')

let flags_of_flow (f : Subject.Statement.Flow.t) : Set.M(M).t =
  add_special_flow_flags f (flags_of_flow_class f)

let pp_set : Set.M(M).t Fmt.t =
  Fmt.(braces (using Set.to_list (list ~sep:comma pp)))

module Flagged = struct
  type 'p t = {path: 'p [@main]; flags: Set.M(M).t [@default (Set.empty (module M))]}
  [@@deriving make, accessors, sexp, compare, equal]

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
