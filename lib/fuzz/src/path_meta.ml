(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Flag = struct
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

  (** Maps a subset of the flags to predicates that toggle whether a piece of
      metadata sets the flag or not. *)
  let metadata_predicates : (t, Metadata.t -> bool) List.Assoc.t =
    [ ( Execute_multi_unsafe
      , Metadata.has_restriction Metadata.Restriction.Once_only )
    ; (In_dead_code, Metadata.(Fn.compose Liveness.is_dead liveness))
      (* We don't add Execute_multi here, as, while it depends on the block
         metadata, it only gets added to loops; consequently, it gets added
         as a special case in the flow flag generator. *) ]

  let flags_of_flow_class' =
    [%accessor
      Accessor.optional_getter (function
        | Fir.Statement_class.Flow.Loop _ ->
            (* Whether or not the loop executes multiple times is stored in
               its block metadata. *)
            Some In_loop
        | Lock (Some Atomic) ->
            Some In_atomic
        | Lock _ | Explicit | Implicit ->
            None)]
end

module Anchor = struct
  (* nb: the compare instance here is NOT inclusion. *)
  type t = Top | Bottom | Full [@@deriving sexp, compare, equal]

  let merge (l : t) (r : t) : t =
    match (l, r) with
    | Top, Top ->
        Top
    | Bottom, Bottom ->
        Bottom
    | _ ->
        Full

  let merge_opt : t option -> t option -> t option = Option.merge ~f:merge

  let incl_opt ?(includes : t option) (x : t option) : bool =
    [%equal: t option] x (merge_opt x includes)

  let of_dimensions ~(span : Utils.My_list.Span.t) ~(block_len : int) :
      t option =
    let top = Option.some_if (0 = span.pos) Top in
    let bot =
      Option.some_if (block_len <= Utils.My_list.Span.end_pos span) Bottom
    in
    merge_opt top bot
end

let flag_contradictions : Set.M(Flag).t list Lazy.t =
  lazy
    (List.map
       ~f:(Set.of_list (module Flag))
       [Flag.[In_execute_multi; Execute_multi_unsafe]])

let flag_contradictions_of_set (xs : Set.M(Flag).t) : Set.M(Flag).t list =
  List.filter (Lazy.force flag_contradictions) ~f:(Set.is_subset ~of_:xs)

let flags_of_metadata (m : Metadata.t) : Set.M(Flag).t =
  Flag.metadata_predicates
  |> List.filter_map ~f:(fun (flag, f) -> Option.some_if (f m) flag)
  |> Set.of_list (module Flag)

let option_map (type a) (o : a Option.t) ~(f : a -> Set.M(Flag).t) :
    Set.M(Flag).t =
  Option.value_map o ~f ~default:(Set.empty (module Flag))

let flags_of_block (b : Subject.Block.t) : Set.M(Flag).t =
  flags_of_metadata b.@(Fir.Block.metadata)

let flags_of_stm (s : Subject.Statement.t) : Set.M(Flag).t =
  s |> Fir.Statement.own_metadata |> option_map ~f:flags_of_metadata

let add_special_flow_flags (f : Subject.Statement.Flow.t)
    (fcf : Set.M(Flag).t) : Set.M(Flag).t =
  let body_m = f.@(Fir.Flow_block.body @> Fir.Block.metadata) in
  if Set.mem fcf In_loop && Metadata.(Liveness.equal Live (liveness body_m))
  then Set.add fcf In_execute_multi
  else fcf

let classify' =
  [%accessor Accessor.optional_getter Fir.Statement_class.Flow.classify]

let flags_of_flow_class : Subject.Statement.Flow.t -> Set.M(Flag).t =
  Accessor.Set.of_accessor
    (module Flag)
    (classify' @> Flag.flags_of_flow_class')

let flags_of_flow (f : Subject.Statement.Flow.t) : Set.M(Flag).t =
  add_special_flow_flags f (flags_of_flow_class f)

let pp_flag_set : Set.M(Flag).t Fmt.t =
  Fmt.(braces (using Set.to_list (list ~sep:comma Flag.pp)))

module Meta = struct
  type t =
    { flags: Set.M(Flag).t
          [@default Set.empty (module Flag)] [@sexp.drop_default.equal]
    ; anchor: Anchor.t option [@sexp.option] }
  [@@deriving accessors, sexp, compare, equal]

  let zero : t = {flags= Set.empty (module Flag); anchor= None}

  let add_flags (x : t) ~(flags : Set.M(Flag).t) : t =
    {x with flags= Set.union x.flags flags}

  let ( + ) (l : t) (r : t) : t =
    { flags= Set.union l.flags r.flags
    ; anchor= Anchor.merge_opt l.anchor r.anchor }

  let check_contradiction_free (m : t) : t Or_error.t =
    let contra = flag_contradictions_of_set m.flags in
    if List.is_empty contra then Ok m
    else
      Or_error.error_s
        [%message
          "Contradiction detected in path flags - possible action generator \
           error"
            ~flags:(m.flags : Set.M(Flag).t)
            ~contradictions:(contra : Set.M(Flag).t list)]

  let pp : t Fmt.t = (* for now *) Fmt.using (fun x -> x.flags) pp_flag_set
end

include Meta

module With_meta = struct
  type 'p t = {path: 'p [@main]; meta: Meta.t [@default Meta.zero]}
  [@@deriving make, accessors, sexp, compare, equal]

  include (
    Travesty.Bi_traversable.Make1_left (struct
      type nonrec 'p t = 'p t

      type right = Meta.t

      module On_monad (Mo : Monad.S) = struct
        let bi_map_m (x : 'a t) ~(left : 'a -> 'b Mo.t)
            ~(right : right -> right Mo.t) : 'b t Mo.t =
          Mo.Let_syntax.(
            let%map path' = left x.path and meta' = right x.meta in
            {path= path'; meta= meta'})
      end
    end) :
      Travesty.Bi_traversable_types.S1_left
        with type 'p t := 'p t
         and type right = Meta.t )
end
