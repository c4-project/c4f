(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {threads: int; returns: int; literal_bools: int} [@@deriving fields]

module Stm = Statement.With_meta (Unit)
module K =
  Travesty.Traversable.Chain0
    (Stm.On_expressions)
    (Expression_traverse.On_constants)

let is_return (stm : Prim_statement.t) : bool =
  Option.exists (Prim_statement.as_early_out stm) ~f:(function
    | Return ->
        true
    | Break | Continue ->
        false)

let fold_fn_decl (stats : t) (decl : Initialiser.t) : t =
  let literal_bools =
    Option.count (Initialiser.value decl) ~f:Constant.is_bool
  in
  {stats with literal_bools= stats.literal_bools + literal_bools}

let fold_fn_decls (decls : Initialiser.t Act_common.C_named.Alist.t)
    (stats : t) : t =
  List.fold decls ~init:stats ~f:(fun stats (_, d) -> fold_fn_decl stats d)

let fold_top_stm (stats : t) (stm : unit Statement.t) : t =
  let returns = Stm.On_primitives.count stm ~f:is_return in
  let literal_bools = K.count stm ~f:Constant.is_bool in
  { stats with
    returns= stats.returns + returns
  ; literal_bools= stats.literal_bools + literal_bools }

let fold_fn_stms (stms : unit Statement.t list) (stats : t) : t =
  List.fold stms ~init:stats ~f:fold_top_stm

let fold_fn (stats : t) (fn : unit Function.t) : t =
  stats
  |> fold_fn_decls (Function.body_decls fn)
  |> fold_fn_stms (Function.body_stms fn)

let fold_named_fn (stats : t) (fn : unit Function.t Act_common.C_named.t) : t
    =
  fold_fn stats (Act_common.C_named.value fn)

let scrape (t : Litmus.Test.t) : t =
  let ts = Litmus.Test.threads t in
  let threads = List.length ts in
  List.fold ts ~init:{threads; returns= 0; literal_bools= 0} ~f:fold_named_fn

let pp : t Fmt.t =
  Fmt.(
    record
      [ field ~sep:sp "threads" threads int
      ; field ~sep:sp "returns" returns int
      ; field ~sep:sp "literal-bools" literal_bools int ])

module Filter :
  Plumbing.Filter_types.S with type aux_i = unit and type aux_o = unit =
Plumbing.Filter.Make (struct
  let name = "dump-stats"

  type aux_i = unit

  type aux_o = unit

  let run (ctx : aux_i Plumbing.Filter_context.t) (ic : Stdio.In_channel.t)
      (oc : Stdio.Out_channel.t) : aux_o Or_error.t =
    Or_error.Let_syntax.(
      let%map test =
        Frontend.load_from_ic ic
          ~path:(Plumbing.Filter_context.input_path_string ctx)
      in
      let f = Caml.Format.formatter_of_out_channel oc in
      Fmt.pf f "@[<v>%a@]@." pp (scrape test))
end)
