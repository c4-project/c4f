(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Context = struct
  type t = {subject: Subject.Test.t; state: State.t; params: Param_map.t}
  [@@deriving accessors, make]
end

module M = Utils.Reader.Fix_context (Utils.Reader.With_errors) (Context)

let lift_state (f : State.t -> 'a) : 'a M.t =
  M.lift (fun {state; _} -> f state)

type t = bool M.t

let always : t = M.return true

let has_threads : t =
  M.lift (fun {subject; _} ->
      subject |> Act_litmus.Test.Raw.threads |> List.is_empty |> not)

let is_filter_constructible (filter : Path_filter.t) ~(kind : Path_kind.t) :
    t =
  M.lift (fun {subject; _} ->
      Path_producers.is_constructible ~filter ~kind subject)

let has_variables ~(predicates : (Var.Record.t -> bool) list) : t =
  (* TODO(@MattWindsor91): this is quite circuitous. Ideally, it should be
     unified with the separate but slightly different 'exists_satisfying_all'
     path. *)
  let f = Travesty_base_exts.List.all ~predicates in
  M.lift (fun {state= {vars; _}; _} ->
      Map.exists ~f (Act_common.Scoped_map.to_litmus_id_map vars))

include (
  struct
    let zero : t = always

    let ( + ) (f : t) (g : t) : t =
      M.(
        let* x = f in
        let+ y = g in
        x && y)
  end :
    Container.Summable with type t := t )
