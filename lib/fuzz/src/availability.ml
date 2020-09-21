(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Context = struct
  type t = {subject: Subject.Test.t; param_map: Param_map.t; state: State.t}
  [@@deriving fields, make]
end

module M =
  Act_utils.Reader.Fix_context (Act_utils.Reader.With_errors) (Context)

type t = bool M.t

let always : t = M.return true

let has_threads : t =
  M.lift (fun ctx ->
      ctx |> Context.subject |> Act_litmus.Test.Raw.threads |> List.is_empty
      |> not)

let is_filter_constructible (filter : Path_filter.t) ~(kind : Path_kind.t) :
    t =
  M.lift (fun ctx ->
      let subject = Context.subject ctx in
      Path_producers.is_constructible ~filter ~kind subject)

let has_variables ~(predicates : (Var.Record.t -> bool) list) : t =
  (* TODO(@MattWindsor91): this is quite circuitous. Ideally, it should be
     unified with the separate but slightly different 'exists_satisfying_all'
     path. *)
  let f = Travesty_base_exts.List.all ~predicates in
  M.lift (fun ctx ->
      ctx |> Context.state |> State.vars
      |> Act_common.Scoped_map.to_litmus_id_map |> Map.exists ~f)

(* TODO(@MattWindsor91): this is a path filter, and should be somewhere else! *)

let threads_of : Set.M(Act_common.Scope).t -> Set.M(Int).t =
  Set.filter_map
    (module Int)
    ~f:(function Act_common.Scope.Global -> None | Local i -> Some i)

let in_thread_with_variables (ctx : Context.t)
    ~(predicates : (Var.Record.t -> bool) list) : Path_filter.t =
  let vars = State.vars (Context.state ctx) in
  let scopes = Var.Map.scopes_with_vars vars ~predicates in
  if Set.mem scopes Global then Path_filter.zero
  else Path_filter.in_threads_only (threads_of scopes)

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
