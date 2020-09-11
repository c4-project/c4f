(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type index = int [@@deriving sexp, compare, equal]

type length = int [@@deriving sexp, compare, equal]

type branch = bool [@@deriving sexp, compare, equal]

type 'a flow_block = In_block of 'a | This_cond
[@@deriving sexp, compare, equal]

type stm =
  | In_if of (bool * stm_list) flow_block
  | In_flow of stm_list flow_block
  | This_stm
[@@deriving sexp, compare, equal]

and stm_list =
  | Insert of index
  | In_stm of index * stm
  | On_range of index * length
[@@deriving sexp, equal]

let bang (type a) (f : Formatter.t) (a : a) : unit = Fmt.(any "@,!@,") f a

let pp_name (type a) fg (step_name : string) : a Fmt.t =
  Fmt.(styled (`Fg fg) (const string step_name))

let pp_step (type a) fg (step_name : string) (pp_rest : a Fmt.t) : a Fmt.t =
  Fmt.(pp_name fg step_name ++ bang ++ pp_rest)

let pp_idx (kind : string) : int Fmt.t =
  Fmt.(const string kind ++ brackets int)

let rec pp_stm (f : Formatter.t) : stm -> unit = function
  | This_stm ->
      pp_name `Green "This" f ()
  | In_if rest ->
      pp_step `Blue "If" pp_if f rest
  | In_flow rest ->
      pp_step `Blue "Flow-block" pp_flow f rest

and pp_stms (f : Formatter.t) : stm_list -> unit = function
  | Insert idx ->
      Fmt.styled (`Fg `Green) (pp_idx "Insert") f idx
  | In_stm (idx, rest) ->
      Fmt.(pair ~sep:bang (styled (`Fg `Cyan) (pp_idx "Stm")) pp_stm)
        f (idx, rest)
  | On_range (idx, stride) ->
      Fmt.(
        styled
          (`Fg `Green)
          (any "Range" ++ brackets (pair ~sep:comma int int)))
        f (idx, stride)

and pp_if (f : Formatter.t) : (bool * stm_list) flow_block -> unit = function
  | In_block (true, rest) ->
      pp_step `Magenta "True" pp_stms f rest
  | In_block (false, rest) ->
      pp_step `Magenta "False" pp_stms f rest
  | This_cond ->
      pp_name `Green "Cond" f ()

and pp_flow (f : Formatter.t) : stm_list flow_block -> unit = function
  | In_block rest ->
      pp_step `Magenta "Body" pp_stms f rest
  | This_cond ->
      pp_name `Green "Cond" f ()

module If = struct
  type t = (bool * stm_list) flow_block [@@deriving sexp, compare, equal]

  let in_branch (b : branch) (rest : stm_list) : t = In_block (b, rest)

  let this_cond : t = This_cond

  let pp : t Fmt.t = pp_if
end

module Flow = struct
  type t = stm_list flow_block [@@deriving sexp, compare, equal]

  let in_body (rest : stm_list) : t = In_block rest

  let this_cond : t = This_cond

  let pp : t Fmt.t = pp_flow
end

module Stm = struct
  type t = stm [@@deriving sexp, compare, equal]

  let in_if (rest : If.t) : t = In_if rest

  let in_flow (rest : Flow.t) : t = In_flow rest

  let this_stm : t = This_stm

  let pp : t Fmt.t = pp_stm
end

module Stms = struct
  type t = stm_list [@@deriving sexp, compare, equal]

  let insert (i : index) : t = Insert i

  let in_stm (i : index) (rest : Stm.t) : t = In_stm (i, rest)

  let stm (i : index) : t = in_stm i Stm.this_stm

  let on_range (i : index) (stride : length) : t = On_range (i, stride)

  let between (start : index) (finish : index) : t =
    on_range start (finish - start)

  let singleton (i : index) : t = On_range (i, 1)

  let pp : t Fmt.t = pp_stms
end

module Thread = struct
  type t = In_stms of stm_list [@@deriving sexp, compare, equal]

  let in_stms (rest : stm_list) : t = In_stms rest

  let pp (f : Formatter.t) : t -> unit = function
    | In_stms rest ->
        Fmt.(styled (`Fg `Yellow) (any "Stms") ++ bang ++ Stms.pp) f rest
end

(* Defined in a separate module to make defining Flagged easier. *)
module Complete = struct
  type t = In_thread of index * Thread.t [@@deriving sexp, compare, equal]

  let in_thread (i : index) (rest : Thread.t) : t = In_thread (i, rest)

  let tid : t -> int = function In_thread (t, _) -> t

  let pp_tid : int Fmt.t = Fmt.(styled (`Fg `Red) (any "P" ++ int))

  let pp (f : Formatter.t) : t -> unit = function
    | In_thread (tid, rest) ->
        Fmt.(pair ~sep:bang pp_tid Thread.pp) f (tid, rest)
end

include Complete

module Flagged = struct
  type t = Complete.t Path_flag.Flagged.t [@@deriving sexp, compare, equal]

  let pp : t Fmt.t =
    Fmt.(
      using Path_flag.Flagged.path Complete.pp
      ++ sp
      ++ braces
           (using
              (Fn.compose Set.to_list Path_flag.Flagged.flags)
              (list ~sep:comma Path_flag.pp)))
end
