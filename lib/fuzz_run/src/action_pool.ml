(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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
        (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t) :
      Fuzz.Action.With_default_weight.t Utils.Weighted_list.t Or_error.t =
    (* TODO(@MattWindsor91): ideally we shouldn't lose whether the weight was
       overridden or not, even if the final weight equals the default one. *)
    weighted_actions
    |> List.map ~f:(fun (act, override) ->
           (act, apply_override act override))
    |> Utils.Weighted_list.from_alist

  let remove (deck : t) ~(name : Common.Id.t) : t Or_error.t =
    (* TODO(@MattWindsor91): this is quite inefficient. *)
    Utils.Weighted_list.adjust_weights deck ~f:(fun a' w ->
        if Common.Id.equal name (Fuzz.Action.With_default_weight.name a')
        then 0
        else w)
end

module Rec_queue = struct
  type t = Fuzz.Action.t Core_kernel.Fqueue.t

  let pick (rq : t) : (Fuzz.Action.t * t) Or_error.t =
    rq |> Core_kernel.Fqueue.dequeue
    |> Result.of_option
         ~error:
           (Error.of_string
              "tried to pick from an empty recommendation list")

  let recommend (rq : t) ~(actions : Fuzz.Action.t list) : t =
    List.fold_left actions ~init:rq ~f:Core_kernel.Fqueue.enqueue
end

type t =
  { deck: Deck.t
  ; original_deck: Deck.t
  ; rec_queue: Rec_queue.t
  ; queue_flag: Fuzz.Flag.t }
[@@deriving accessors]

let reset (x : t) : t = {x with deck= x.original_deck}

let find_in_originals ({original_deck; _} : t) ~(name : Common.Id.t) :
    Fuzz.Action.t Or_error.t =
  original_deck
  |> Utils.Weighted_list.find ~f:(fun x ->
         Common.Id.equal name (Fuzz.Action.With_default_weight.name x))
  |> Option.map ~f:Fuzz.Action.With_default_weight.action
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message
              "Tried to recommend an action that isn't available"
                ~name:(name : Common.Id.t)])

let find_many_in_originals (pool : t) ~(names : Common.Id.t list) :
    Fuzz.Action.t list Or_error.t =
  Or_error.combine_errors
    (List.map names ~f:(fun name -> find_in_originals pool ~name))

let recommend (x : t) ~(names : Common.Id.t list) : t Or_error.t =
  Or_error.Let_syntax.(
    let%map actions = find_many_in_originals x ~names in
    Accessor.map rec_queue x ~f:(Rec_queue.recommend ~actions))

let of_weighted_actions
    (weighted_actions :
      (Fuzz.Action.With_default_weight.t, int option) List.Assoc.t)
    ~(queue_flag : Fuzz.Flag.t) : t Or_error.t =
  Or_error.(
    Deck.of_weighted_actions weighted_actions
    >>| fun deck ->
    { deck
    ; original_deck= deck
    ; queue_flag
    ; rec_queue= Core_kernel.Fqueue.empty })

let use_queue ({rec_queue; queue_flag; _} : t)
    ~(random : Splittable_random.State.t) : bool =
  (not (Core_kernel.Fqueue.is_empty rec_queue))
  && Fuzz.Flag.eval queue_flag ~random

let pick_without_remove (table : t) ~(random : Splittable_random.State.t) :
    (Fuzz.Action.t * Rec_queue.t) Or_error.t =
  if use_queue table ~random then Rec_queue.pick table.rec_queue
  else
    Or_error.(
      Utils.Weighted_list.sample table.deck ~random
      >>| fun a -> (Fuzz.Action.With_default_weight.action a, table.rec_queue))

let pick (table : t) ~(random : Splittable_random.State.t) :
    (Fuzz.Action.t * t) Or_error.t =
  Or_error.Let_syntax.(
    let%bind (module A), queue' = pick_without_remove table ~random in
    (* We always remove the chosen action from the deck, even if it came from
       a recommendation. This is to make sure that, if a recommendation fails
       for whatever reason, we don't immediately try picking it again. *)
    let%map deck' = Deck.remove table.deck ~name:A.name in
    ( (module A : Fuzz.Action_types.S)
    , {table with deck= deck'; rec_queue= queue'} ))
