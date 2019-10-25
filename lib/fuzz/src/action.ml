(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common
open Act_utils

type t = (module Action_types.S)

module With_default_weight = struct
  type t = {action: (module Action_types.S); default_weight: int}
  [@@deriving make, fields]

  let name ({action= (module M); _} : t) : Act_common.Id.t = M.name

  let available ({action= (module M); _} : t) :
      Subject.Test.t -> bool State.Monad.t =
    M.available

  let zero_if_not_available (subject : Subject.Test.t) (action : t)
      (weight : int) : int State.Monad.t =
    State.Monad.Let_syntax.(
      if%map available action subject then weight else 0)
end

let lift_quickcheck (type a) (gen : a Base_quickcheck.Generator.t)
    ~(random : Splittable_random.State.t) : a State.Monad.t =
  State.Monad.return
    (Base_quickcheck.Generator.generate ~size:10 ~random gen)

let always : Subject.Test.t -> bool State.Monad.t =
  Fn.const (State.Monad.return true)

module Adjusted_weight = struct
  type t = Not_adjusted of int | Adjusted of {original: int; actual: int}

  let make ?(user_weight : int option) ~(default_weight : int) : t =
    Option.value_map user_weight
      ~f:(fun actual -> Adjusted {original= default_weight; actual})
      ~default:(Not_adjusted default_weight)

  let pp_weight (f : Formatter.t) : int -> unit =
    Fmt.(
      function
      | 0 ->
          styled (`Fg `Red) (any "disabled") f ()
      | k ->
          styled (`Fg `Green) (int ++ any "x") f k)

  let pp (f : Formatter.t) : t -> unit = function
    | Not_adjusted o ->
        pp_weight f o
    | Adjusted {original; actual} ->
        Fmt.pf f "%a (normally %a)" pp_weight actual pp_weight original
end

module Summary = struct
  type t = {weight: Adjusted_weight.t; readme: string}
  [@@deriving fields, make]

  let of_action ?(user_weight : int option)
      ({action= (module M); default_weight} : With_default_weight.t) : t =
    let weight = Adjusted_weight.make ~default_weight ?user_weight in
    let readme = M.readme () in
    {weight; readme}

  let pp (f : Formatter.t) ({weight; readme} : t) : unit =
    Fmt.pf f "@[<v>@[Weight:@ %a@]@,@[<hv 2>Summary:@ @[%a@]@]@]"
      Adjusted_weight.pp weight Fmt.paragraphs readme

  let pp_map : t Map.M(Act_common.Id).t Fmt.t = Act_common.Id.pp_map pp
end

module Pool = struct
  type t = With_default_weight.t Weighted_list.t

  let of_weighted_actions
      (weighted_actions : (With_default_weight.t, int option) List.Assoc.t)
      : t Or_error.t =
    (* TODO(@MattWindsor91): ideally we shouldn't lose whether the weight
       was overridden or not, even if the final weight equals the default
       one. *)
    weighted_actions
    |> List.map ~f:(fun (act, override) ->
           ( act
           , Option.value override
               ~default:(With_default_weight.default_weight act) ))
    |> Weighted_list.from_alist

  let summarise : t -> Summary.t Id.Map.t =
    Weighted_list.fold ~init:Id.Map.empty
      ~f:(fun map (action : With_default_weight.t) weight ->
        (* TODO(@MattWindsor91): see TODO above, as it pertains to this bit
           too. *)
        let user_weight =
          if weight = With_default_weight.default_weight action then None
          else Some weight
        in
        Map.set map
          ~key:(With_default_weight.name action)
          ~data:(Summary.of_action action ?user_weight))

  module W = Weighted_list.On_monad (struct
    include State.Monad

    let lift = State.Monad.Monadic.return
  end)

  (** [to_available_only wl ~subject] is a stateful action that modifies
      [wl] to pull any actions not available on [subject] to weight 0. *)
  let to_available_only (wl : t) ~(subject : Subject.Test.t) :
      t State.Monad.t =
    W.adjust_weights_m wl
      ~f:(With_default_weight.zero_if_not_available subject)

  let pick_from_available (available : t)
      ~(random : Splittable_random.State.t) :
      (module Action_types.S) State.Monad.t =
    State.Monad.(
      available
      |> Weighted_list.sample ~random
      |> Monadic.return >>| With_default_weight.action)

  let pick (table : t) ~(subject : Subject.Test.t)
      ~(random : Splittable_random.State.t) :
      (module Action_types.S) State.Monad.t =
    State.Monad.(
      table |> to_available_only ~subject >>= pick_from_available ~random)
end

module Pure_payload (S : sig
  type t [@@deriving sexp]

  val quickcheck_generator : t Base_quickcheck.Generator.t
end) : Action_types.S_payload with type t = S.t = struct
  include S

  let gen (_subject : Subject.Test.t) ~(random : Splittable_random.State.t)
      : t State.Monad.t =
    lift_quickcheck S.quickcheck_generator ~random
end

module No_payload : Action_types.S_payload with type t = unit = struct
  include Unit

  let gen (_ : Subject.Test.t) ~(random : Splittable_random.State.t) =
    ignore (random : Splittable_random.State.t) ;
    State.Monad.return ()
end

module Make_log (B : sig
  val name : Act_common.Id.t
end) : sig
  val log : Act_common.Output.t -> string -> unit
end = struct
  let log (o : Act_common.Output.t) (s : string) : unit =
    Fmt.(
      Act_common.Output.pv o "@[<h>%a:@ @[%a@]@]@."
        (styled (`Fg `Green) Act_common.Id.pp)
        B.name Fmt.lines s)
end
