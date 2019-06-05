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

open Core_kernel (* not Base; for Time.Span.t *)

open Act_common
module Au = Act_utils
include Analysis_intf

module Herd = struct
  type t = Run of Act_sim.Diff.t | Disabled | Errored of [`C | `Assembly]

  let to_string : t -> string = function
    | Errored `Assembly ->
        "ERROR (asm)"
    | Errored `C ->
        "ERROR (C)"
    | Disabled ->
        "--disabled--"
    | Run x ->
        Act_sim.Diff.to_string x

  let pp : t Fmt.t = Fmt.of_to_string to_string

  module Predicates = struct
    let is_undefined : t -> bool = function
      | Run Oracle_undefined | Run Subject_undefined ->
          true
      | Run _ | Errored _ | Disabled ->
          false

    let has_errors : t -> bool = function
      | Errored _ ->
          true
      | Run _ | Disabled ->
          false
  end

  include Predicates

  let state_set_order : t -> Act_sim.Diff.Order.t option = function
    | Run (Result r) ->
        Some r
    | Run _ | Errored _ | Disabled ->
        None

  (** Forwards from [Sim.Diff.Order]. *)
  module Order_forwards = struct
    module H = Act_utils.Inherit.Partial_helpers (struct
      type nonrec t = t

      type c = Act_sim.Diff.Order.t

      let component_opt : t -> c option = state_set_order
    end)

    let has_deviations : t -> bool =
      H.forward_bool (Fn.non Au.Set_partial_order.is_equal)

    let has_asm_deviations : t -> bool =
      H.forward_bool Au.Set_partial_order.right_has_uniques
  end

  include Order_forwards
end

module File = struct
  type t =
    { time_taken: Time.Span.t option
    ; time_taken_in_cc: Time.Span.t option
    ; herd: Herd.t }
  [@@deriving fields, make]

  module Herd_forwards = struct
    module H = Act_utils.Inherit.Helpers (struct
      type nonrec t = t

      type c = Herd.t

      let component : t -> c = herd
    end)

    let state_set_order : t -> Act_sim.Diff.Order.t option =
      H.forward Herd.state_set_order

    let has_deviations : t -> bool = H.forward Herd.has_deviations

    let has_asm_deviations : t -> bool = H.forward Herd.has_asm_deviations

    let has_errors : t -> bool = H.forward Herd.has_errors

    let is_undefined : t -> bool = H.forward Herd.is_undefined
  end

  include Herd_forwards
end

module Compiler = struct
  type t =
    {time_taken: Time.Span.t option; files: (string, File.t) List.Assoc.t}
  [@@deriving fields, make]
end

module Machine = struct
  type t =
    { time_taken: Time.Span.t option
    ; compilers: (Id.t, Compiler.t) List.Assoc.t }
  [@@deriving fields, make]

  let files m =
    List.concat_map (compilers m) ~f:(fun (cid, compiler) ->
        List.map (Compiler.files compiler) ~f:(fun (fname, analysis) ->
            (cid, fname, analysis) ) )
end

type t =
  {time_taken: Time.Span.t option; machines: (Id.t, Machine.t) List.Assoc.t}
[@@deriving fields, make]
