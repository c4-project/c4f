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

let lift_quickcheck (type a) (gen : a Base_quickcheck.Generator.t)
    ~(random : Splittable_random.State.t) : a State.Monad.t =
  State.Monad.return
    (Base_quickcheck.Generator.generate ~size:10 ~random gen)

let always : Subject.Test.t -> bool State.Monad.t =
  Fn.const (State.Monad.return true)

let zero_if_not_available (subject : Subject.Test.t)
    (module A : Action_types.S) (weight : int) : int State.Monad.t =
  State.Monad.Let_syntax.(if%map A.available subject then weight else 0)

module Adjusted_weight = struct
  type t = Not_adjusted of int | Adjusted of {original: int; actual: int}

  let make (module M : Action_types.S) (user_weight : int) : t =
    if user_weight = M.default_weight then Not_adjusted M.default_weight
    else Adjusted {original= M.default_weight; actual= user_weight}

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

  let of_action (module M : Action_types.S) ~(user_weight : int) : t =
    let weight =
      Adjusted_weight.make (module M : Action_types.S) user_weight
    in
    let readme = M.readme () in
    {weight; readme}

  let pp (f : Formatter.t) ({weight; readme} : t) : unit =
    Fmt.pf f "@[<v>@[Weight:@ %a@]@,@[<hv 2>Summary:@ @[%a@]@]@]"
      Adjusted_weight.pp weight Fmt.paragraphs readme
end

module Pool = struct
  type t = (module Action_types.S) Weighted_list.t

  let of_weighted_actions
      (weighted_actions : ((module Action_types.S), int) List.Assoc.t) :
      t Or_error.t =
    Weighted_list.from_alist weighted_actions

  let summarise : t -> Summary.t Id.Map.t =
    Weighted_list.fold ~init:Id.Map.empty
      ~f:(fun map (module M : Action_types.S) user_weight ->
        Map.set map ~key:M.name
          ~data:(Summary.of_action (module M) ~user_weight))

  module W = Weighted_list.On_monad (struct
    include State.Monad

    let lift = State.Monad.Monadic.return
  end)

  (** [to_available_only wl ~subject] is a stateful action that modifies
      [wl] to pull any actions not available on [subject] to weight 0. *)
  let to_available_only (wl : t) ~(subject : Subject.Test.t) :
      t State.Monad.t =
    W.adjust_weights_m wl ~f:(zero_if_not_available subject)

  let pick (table : t) (subject : Subject.Test.t)
      (random : Splittable_random.State.t) :
      (module Action_types.S) State.Monad.t =
    State.Monad.(
      table
      |> to_available_only ~subject
      >>= Fn.compose State.Monad.Monadic.return
            (Weighted_list.sample ~random))
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
