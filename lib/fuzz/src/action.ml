(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

open Base
open Act_common
open Act_utils

let zero_if_not_available (subject : Subject.Test.t)
    (module A : Action_types.S) (weight : int) : int State.Monad.t =
  let open State.Monad.Let_syntax in
  if%map A.available subject then weight else 0

module Adjusted_weight = struct
  type t = Not_adjusted of int | Adjusted of {original: int; actual: int}

  let make (module M : Action_types.S) (user_weight : int) : t =
    if user_weight = M.default_weight then Not_adjusted M.default_weight
    else Adjusted {original= M.default_weight; actual= user_weight}

  let pp_weight (f : Formatter.t) : int -> unit =
    Fmt.(
      function
      | 0 ->
          styled_unit `Red "disabled" f ()
      | k ->
          styled `Green (suffix (unit "x") int) f k)

  let pp (f : Formatter.t) : t -> unit = function
    | Not_adjusted o ->
        pp_weight f o
    | Adjusted {original; actual} ->
        Fmt.pf f "%a (normally %a)" pp_weight actual pp_weight original
end

module Summary = struct
  type t = {weight: Adjusted_weight.t; readme: string} [@@deriving fields]

  let make (module M : Action_types.S) (user_weight : int) : t =
    let weight =
      Adjusted_weight.make (module M : Action_types.S) user_weight
    in
    let readme = M.readme () in
    {weight; readme}

  let pp (f : Formatter.t) ({weight; readme} : t) : unit =
    Fmt.pf f "@[<v>@[Weight:@ %a@]@,@[<hv 2>Summary:@ @[%a@]@]@]"
      Adjusted_weight.pp weight Fmt.paragraphs readme

  let%expect_test "pp: example summary" =
    let weight = Adjusted_weight.(Adjusted {original= 27; actual= 53}) in
    let readme =
      {|
        Why, hello there!  This is an example README.

        It is very long...

        ...and has line breaks and other such interesting things in it.
        Hopefully, it'll be enough to be able to test that the action
        summary pretty-printer does what it's supposed to.

        Be seeing you.
      |}
    in
    let summary = {weight; readme} in
    Fmt.pr "%a@." pp summary ;
    [%expect
      {|
      Weight: 53x (normally 27x)
      Summary:
        Why, hello there! This is an example README.

        It is very long...

        ...and has line breaks and other such interesting things in it. Hopefully,
        it'll be enough to be able to test that the action summary pretty-printer
        does what it's supposed to.

        Be seeing you. |}]
end

module Pool = struct
  type t = (module Action_types.S) Weighted_list.t

  let make_weight_pair (weight_overrides : int Id.Map.t)
      (module M : Action_types.S) : (module Action_types.S) * int =
    let weight =
      M.name
      |> Id.Map.find weight_overrides
      |> Option.value ~default:M.default_weight
    in
    ((module M : Action_types.S), weight)

  let make_weight_alist (actions : (module Action_types.S) list)
      (weight_overrides : int Id.Map.t) :
      ((module Action_types.S), int) List.Assoc.t =
    List.map ~f:(make_weight_pair weight_overrides) actions

  let make (actions : (module Action_types.S) list)
      (config : Act_config.Fuzz.t) : t Or_error.t =
    let weight_overrides_alist = Act_config.Fuzz.weights config in
    Or_error.Let_syntax.(
      let%bind weight_overrides =
        Id.Map.of_alist_or_error weight_overrides_alist
      in
      let weights = make_weight_alist actions weight_overrides in
      Weighted_list.from_alist weights)

  let summarise : t -> Summary.t Id.Map.t =
    Weighted_list.fold ~init:Id.Map.empty
      ~f:(fun map (module M : Action_types.S) user_weight ->
        Map.set map ~key:M.name ~data:(Summary.make (module M) user_weight))

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
