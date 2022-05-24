(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Deck = struct
  type t = Fuzz.Action.With_default_weight.t Utils.Weighted_list.t

  let apply_override (act : Fuzz.Action.With_default_weight.t)
      (override : int option) : int =
    Option.value override
      ~default:(Fuzz.Action.With_default_weight.default_weight act)

  let of_weighted_actions
      (weighted_actions :
        (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t ) :
      Fuzz.Action.With_default_weight.t Utils.Weighted_list.t Or_error.t =
    (* TODO(@MattWindsor91): ideally we shouldn't lose whether the weight was
       overridden or not, even if the final weight equals the default one. *)
    weighted_actions
    |> List.map ~f:(fun (act, override) ->
           (act, apply_override act override) )
    |> Utils.Weighted_list.from_alist

  let remove (deck : t) ~(name : Common.Id.t) : t Or_error.t =
    (* TODO(@MattWindsor91): this is quite inefficient. *)
    Utils.Weighted_list.adjust_weights deck ~f:(fun a' w ->
        if Common.Id.equal name (Fuzz.Action.With_default_weight.name a')
        then 0
        else w )
end

module Rec_queue = struct
  type t = Fuzz.Action.t Core.Fqueue.t

  let pick (rq : t) : Fuzz.Action.t option * t =
    match Core.Fqueue.dequeue rq with
    | Some (a, rq') -> (Some a, rq')
    | None -> (None, rq)

  let maybe_enqueue (q : t) (action : Fuzz.Action.t) ~(flag : Fuzz.Flag.t)
      ~(random : Splittable_random.State.t) : t =
    if Fuzz.Flag.eval flag ~random then Core.Fqueue.enqueue q action else q

  let recommend (rq : t) ~(actions : Fuzz.Action.t list)
      ~(flag : Fuzz.Flag.t) ~(random : Splittable_random.State.t) : t =
    List.fold_left actions ~init:rq ~f:(maybe_enqueue ~flag ~random)
end

type t =
  { deck: Deck.t
  ; original_deck: Deck.t
  ; rec_queue: Rec_queue.t
  ; accept_rec_flag: Fuzz.Flag.t
  ; use_rec_flag: Fuzz.Flag.t }
[@@deriving accessors]

let reset (x : t) : t = {x with deck= x.original_deck}

let find_in_originals ({original_deck; _} : t) ~(name : Common.Id.t) :
    Fuzz.Action.t option =
  (* Not finding a recommendation isn't an error; the deck might've been
     subsampled, and the recommendation action might have been removed. *)
  original_deck
  |> Utils.Weighted_list.find ~f:(fun x ->
         Common.Id.equal name (Fuzz.Action.With_default_weight.name x) )
  |> Option.map ~f:Fuzz.Action.With_default_weight.action

let find_many_in_originals (pool : t) ~(names : Common.Id.t list) :
    Fuzz.Action.t list =
  List.filter_map names ~f:(fun name -> find_in_originals pool ~name)

let recommend (x : t) ~(names : Common.Id.t list)
    ~(random : Splittable_random.State.t) : t =
  let actions = find_many_in_originals x ~names in
  Accessor.map rec_queue x
    ~f:(Rec_queue.recommend ~actions ~flag:x.accept_rec_flag ~random)

let of_weighted_actions
    (weighted_actions :
      (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t )
    ~(accept_rec_flag : Fuzz.Flag.t) ~(use_rec_flag : Fuzz.Flag.t) :
    t Or_error.t =
  Or_error.(
    Deck.of_weighted_actions weighted_actions
    >>| fun deck ->
    { deck
    ; original_deck= deck
    ; accept_rec_flag
    ; use_rec_flag
    ; rec_queue= Core.Fqueue.empty })

let use_queue ({rec_queue; use_rec_flag; _} : t)
    ~(random : Splittable_random.State.t) : bool =
  (not (Core.Fqueue.is_empty rec_queue))
  && Fuzz.Flag.eval use_rec_flag ~random

let pick_without_remove (table : t) ~(random : Splittable_random.State.t) :
    Fuzz.Action.t option * t =
  if use_queue table ~random then
    let ao, queue' = Rec_queue.pick table.rec_queue in
    (ao, {table with rec_queue= queue'})
  else
    let wlo = Utils.Weighted_list.sample table.deck ~random in
    (Option.map wlo ~f:Fuzz.Action.With_default_weight.action, table)

let maybe_remove (table : t) : Fuzz.Action.t option -> t Or_error.t =
  function
  | None -> Ok table
  | Some (module A) ->
      Utils.Accessor.On_error.map deck table ~f:(Deck.remove ~name:A.name)

let pick (table : t) ~(random : Splittable_random.State.t) :
    (Fuzz.Action.t option * t) Or_error.t =
  let ao, table = pick_without_remove table ~random in
  Or_error.Let_syntax.(
    (* We always remove the chosen action from the deck, even if it came from
       a recommendation. This is to make sure that, if a recommendation fails
       for whatever reason, we don't immediately try picking it again. *)
    let%map table = maybe_remove table ao in
    (ao, table))

let pick_many (pool : t) ~(max : int) ~(random : Splittable_random.State.t) :
    (Fuzz.Action.t list * t) Or_error.t =
  Or_error.Let_syntax.(
    let%map pool, xs =
      Travesty_base_exts.List.With_errors.fold_map_m
        (List.init max ~f:(fun _ -> ()))
        ~init:pool
        ~f:(fun pool () ->
          let%map ao, pool' = pick pool ~random in
          (pool', ao) )
    in
    (List.filter_opt xs, pool))
