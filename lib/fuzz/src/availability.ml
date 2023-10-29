(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Context = struct
  type t = {subject: Subject.Test.t; state: State.t; params: Param_map.t}
  [@@deriving accessors, make]
end

module M = struct
  include Utils.Reader.Fix_context (Utils.Reader.With_errors) (Context)

  let lift_acc acc = lift (Accessor.get acc)

  let lift_state (f : State.t -> 'a) : 'a t =
    lift_acc Accessor.(Context.state @> getter f)

  let param (id : Common.Id.t) : int t =
    let* ps = lift_acc Context.params in
    Inner.return (Param_map.get_param ps ~id)
end

type t = bool M.t

let always : t = M.return true

let has_threads : t =
  M.lift (fun {subject; _} ->
      subject |> C4f_litmus.Test.Raw.threads |> List.is_empty |> not )

let is_filter_constructible (filter : Path_filter.t) ~(kind : Path_kind.t) :
    t =
  M.lift (fun {subject; _} ->
      Path_producers.is_constructible ~filter ~kind subject )

let has_variables ~(predicates : (Var.Record.t -> bool) list) : t =
  (* TODO(@MattWindsor91): this is quite circuitous. Ideally, it should be
     unified with the separate but slightly different 'exists_satisfying_all'
     path. *)
  let f = Travesty_base_exts.List.all ~predicates in
  M.lift (fun {state= {vars; _}; _} ->
      Map.exists ~f (C4f_common.Scoped_map.to_litmus_id_map vars) )

let in_var_cap ~(after_adding : int) : t =
  if after_adding <= 0 then M.return true
  else
    M.(
      let* cap = param Config_tables.var_cap_param in
      let+ vars = lift_acc (Context.state @> State.vars) in
      Common.Scoped_map.length vars + after_adding <= cap )

include (
  struct
    let zero : t = always

    let ( + ) (f : t) (g : t) : t =
      M.(
        let* x = f in
        let+ y = g in
        x && y )
  end :
    Container.Summable with type t := t )
