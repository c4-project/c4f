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

type t = Context.t -> bool Or_error.t

let always : t = Fn.const (Ok true)

let has_threads (ctx : Context.t) : bool Or_error.t =
  Ok
    ( ctx |> Context.subject |> Act_litmus.Test.Raw.threads |> List.is_empty
    |> not )

let is_filter_constructible (filter : Path_filter.t) ~(kind : Path_kind.t)
    (ctx : Context.t) : bool Or_error.t =
  let subject = Context.subject ctx in
  Ok (Path_producers.is_constructible ~filter ~kind subject)

include (struct
  let zero : t = always
  let (+) (f : t) (g : t) : t =
    fun ctx -> Or_error.map2 (f ctx) (g ctx) ~f:(fun x y -> x && y)
end : Container.Summable with type t := t)