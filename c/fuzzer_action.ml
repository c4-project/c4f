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
open Utils
include Fuzzer_action_intf

let zero_if_not_available (subject : Fuzzer_subject.Test.t) (module A : S)
    (weight : int) : int Fuzzer_state.Monad.t =
  let open Fuzzer_state.Monad.Let_syntax in
  if%map A.available subject then weight else 0

module Pool = struct
  type t = (module S) Weighted_list.t

  let make_weight_pair (weight_overrides : int Config.Id.Map.t) (module M : S)
    : ((module S) * int) =
    let weight =
      M.name
      |> Config.Id.Map.find weight_overrides
      |> Option.value ~default:(M.default_weight)
    in
    ((module M : S), weight)

  let make_weight_alist (actions : (module S) list) (weight_overrides : int Config.Id.Map.t)
    : ((module S), int) List.Assoc.t =
    List.map ~f:(make_weight_pair weight_overrides) actions

  let make (actions : (module S) list) (config : Config.Fuzz.t)
    : t Or_error.t =
    let weight_overrides_alist = Config.Fuzz.weights config in
    Or_error.Let_syntax.(
      let%bind weight_overrides = Config.Id.Map.of_alist_or_error weight_overrides_alist in
      let weights = make_weight_alist actions weight_overrides in
      Weighted_list.from_alist weights
    )

  module W = Weighted_list.On_monad (struct
    include Fuzzer_state.Monad

    let lift = Fuzzer_state.Monad.Monadic.return
  end)

  (** [to_available_only wl subject] is a stateful action that modifies [wl]
      to pull any actions not available on [subject] to weight 0. *)
  let to_available_only (wl : t) (subject : Fuzzer_subject.Test.t) :
      t Fuzzer_state.Monad.t =
    W.adjust_weights_m wl ~f:(zero_if_not_available subject)

  let pick (table : t) (subject : Fuzzer_subject.Test.t)
      (random : Splittable_random.State.t) : (module S) Fuzzer_state.Monad.t
      =
    let open Fuzzer_state.Monad.Let_syntax in
    let%bind available = to_available_only table subject in
    Fuzzer_state.Monad.Monadic.return
      (Weighted_list.sample available ~random)
end
