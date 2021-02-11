(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* We define sexp conversion on paths through fragments. *)

type index = int [@@deriving sexp, compare, equal]

type length = int [@@deriving sexp, compare, equal]

type branch = bool [@@deriving sexp, compare, equal]

module Fragment = struct
  type t =
    | Branch of branch
    | Body
    | Cond
    | Flow
    | If
    | Insert of index
    | Range of index * length
    | Stm of index
    | Stms
    | This
    | Thread of int
  [@@deriving sexp]

  let pp_name (type a) fg (step_name : string) : a Fmt.t =
    Fmt.(styled (`Fg fg) (const string step_name))

  let pp_idx fg (kind : string) : int Fmt.t =
    Fmt.(pp_name fg kind ++ brackets int)

  let pp (f : Formatter.t) : t -> unit = function
    | Branch false -> pp_name `Magenta "False" f ()
    | Branch true -> pp_name `Magenta "False" f ()
    | Body -> pp_name `Magenta "Body" f ()
    | Cond -> pp_name `Green "Cond" f ()
    | Flow -> pp_name `Blue "Flow" f ()
    | If -> pp_name `Blue "If" f ()
    | Insert idx -> pp_idx `Green "Insert" f idx
    | Range (idx, stride) ->
        Fmt.(
          styled
            (`Fg `Green)
            (any "Range" ++ brackets (pair ~sep:comma int int)))
          f (idx, stride)
    | Stm idx -> pp_idx `Cyan "Stm" f idx
    | Stms -> pp_name `Yellow "Stms" f ()
    | This -> pp_name `Green "This" f ()
    | Thread idx -> pp_idx `Red "Thread" f idx

  let bad_fragments (sort : string) (hd : t) (fs : t list) : 'a Or_error.t =
    Or_error.error_s
      [%message
        "Path fragments are not valid for the expected type of path"
          ~fragments:(hd :: fs : t list)
          ~sort]

  let not_enough (sort : string) : 'a Or_error.t =
    Or_error.error_s [%message "Expected more path fragments here" ~sort]

  let terminate (sort : string) (result : 'a) (overflow : t list) :
      'a Or_error.t =
    if List.is_empty overflow then Ok result
    else
      Or_error.error_s
        [%message
          "Too many path fragments in list"
            ~overflow:(overflow : t list)
            ~sort]

  let lift_not_enough (sort : string) (xs : t list)
      (f : t -> t list -> 'a Or_error.t) : 'a Or_error.t =
    match xs with hd :: tl -> f hd tl | [] -> not_enough sort

  let lift_continue (type a b) (f : t list -> a Or_error.t) (g : a -> b)
      (rest : t list) : b Or_error.t =
    Or_error.(f rest >>| g)
end

type 'a flow_block = In_block of 'a | This_cond
[@@deriving sexp, compare, equal]

type stm =
  | In_if of (bool * stm_list) flow_block
  | In_flow of stm_list flow_block
  | This_stm
[@@deriving compare, equal]

and stm_list =
  | Insert of index
  | In_stm of index * stm
  | On_range of index * length
[@@deriving equal]

let rec flow_block_of_fragments (rest : Fragment.t list) :
    stm_list flow_block Or_error.t =
  Fragment.(
    lift_not_enough "flow" rest
    @@ function
    | Body -> lift_continue stm_list_of_fragments (fun r -> In_block r)
    | Cond -> terminate "flow" This_cond
    | x -> bad_fragments "flow" x)

and if_of_fragments (rest : Fragment.t list) :
    (bool * stm_list) flow_block Or_error.t =
  Fragment.(
    lift_not_enough "if" rest
    @@ function
    | Branch b ->
        lift_continue stm_list_of_fragments (fun r -> In_block (b, r))
    | Cond -> terminate "if" This_cond
    | x -> bad_fragments "if" x)

and stm_of_fragments (rest : Fragment.t list) : stm Or_error.t =
  Fragment.(
    lift_not_enough "statement" rest
    @@ function
    | Flow -> lift_continue flow_block_of_fragments (fun r -> In_flow r)
    | If -> lift_continue if_of_fragments (fun r -> In_if r)
    | This -> terminate "statement" This_stm
    | x -> bad_fragments "statement" x)

and stm_list_of_fragments (rest : Fragment.t list) : stm_list Or_error.t =
  Fragment.(
    lift_not_enough "statement-list" rest
    @@ function
    | Insert i -> terminate "statement-list" (Insert i : stm_list)
    | Stm i -> lift_continue stm_of_fragments (fun r -> In_stm (i, r))
    | Range (i, s) -> terminate "statement-list" (On_range (i, s))
    | x -> bad_fragments "statement-list" x)

let rec fragments_of_flow_block (x : stm_list flow_block) : Fragment.t list =
  match x with
  | In_block rest -> Body :: fragments_of_stm_list rest
  | This_cond -> [Cond]

and fragments_of_if (x : (bool * stm_list) flow_block) : Fragment.t list =
  match x with
  | In_block (b, rest) -> Branch b :: fragments_of_stm_list rest
  | This_cond -> [Cond]

and fragments_of_stm (x : stm) : Fragment.t list =
  match x with
  | In_flow rest -> Flow :: fragments_of_flow_block rest
  | In_if rest -> If :: fragments_of_if rest
  | This_stm -> [This]

and fragments_of_stm_list (xs : stm_list) : Fragment.t list =
  match xs with
  | Insert i -> [Insert i]
  | In_stm (i, rest) -> Stm i :: fragments_of_stm rest
  | On_range (i, len) -> [Range (i, len)]

module If = struct
  type t = (bool * stm_list) flow_block [@@deriving compare, equal]

  let in_branch (b : branch) (rest : stm_list) : t = In_block (b, rest)

  let this_cond : t = This_cond
end

module Flow = struct
  type t = stm_list flow_block [@@deriving compare, equal]

  let in_body (rest : stm_list) : t = In_block rest

  let this_cond : t = This_cond
end

module Stm = struct
  type t = stm [@@deriving compare, equal]

  let in_if (rest : If.t) : t = In_if rest

  let in_flow (rest : Flow.t) : t = In_flow rest

  let this_stm : t = This_stm
end

module Stms = struct
  type t = stm_list [@@deriving compare, equal]

  let index : type i. (i, index, t, [< field]) Accessor.Simple.t =
    [%accessor
      Accessor.field
        ~get:(function
          | Insert i -> i | In_stm (i, _) -> i | On_range (i, _) -> i )
        ~set:(fun x i ->
          match x with
          | Insert _ -> Insert i
          | In_stm (_, r) -> In_stm (i, r)
          | On_range (_, s) -> On_range (i, s) )]

  let len : t -> int = function
    | Insert _ -> 0
    | In_stm _ -> 1
    | On_range (_, l) -> l

  (* TODO(@MattWindsor91): solve pos/index inconsistency*)
  let span (p : t) : Utils.My_list.Span.t = {pos= p.@(index); len= len p}

  let is_nested : t -> bool = function
    | In_stm (_, In_if _) | In_stm (_, In_flow _) -> true
    | In_stm (_, This_stm) | Insert _ | On_range _ -> false

  let insert (i : index) : t = Insert i

  let in_stm (i : index) (rest : Stm.t) : t = In_stm (i, rest)

  let stm (i : index) : t = in_stm i Stm.this_stm

  let on_range (i : index) (stride : length) : t = On_range (i, stride)

  let between (start : index) (finish : index) : t =
    on_range start (finish - start)

  let singleton (i : index) : t = On_range (i, 1)

  let sexp_of_t (path : t) : Sexp.t =
    List.sexp_of_t Fragment.sexp_of_t (fragments_of_stm_list path)

  let t_of_sexp (sexp : Sexp.t) : t =
    let frags = List.t_of_sexp Fragment.t_of_sexp sexp in
    Or_error.ok_exn (stm_list_of_fragments frags)
end

module Thread = struct
  type t = In_stms of stm_list [@@deriving compare, equal]

  let in_stms (rest : stm_list) : t = In_stms rest

  let of_fragments (frags : Fragment.t list) : t Or_error.t =
    Fragment.(
      lift_not_enough "thread" frags
      @@ function
      | Stms -> lift_continue stm_list_of_fragments (fun r -> In_stms r)
      | x -> bad_fragments "thread" x)

  let fragments_of : t -> Fragment.t list = function
    | In_stms rest -> Stms :: fragments_of_stm_list rest
end

(* Defined in a separate module to make defining Flagged easier. *)
module Complete = struct
  type t = In_thread of index * Thread.t [@@deriving compare, equal]

  let in_thread (i : index) (rest : Thread.t) : t = In_thread (i, rest)

  let tid : t -> int = function In_thread (t, _) -> t

  let of_fragments (frags : Fragment.t list) : t Or_error.t =
    Fragment.(
      lift_not_enough "program" frags
      @@ function
      | Thread n ->
          lift_continue Thread.of_fragments (fun r -> In_thread (n, r))
      | x -> bad_fragments "program" x)

  let fragments_of : t -> Fragment.t list = function
    | In_thread (n, rest) -> Thread n :: Thread.fragments_of rest

  let sexp_of_t (path : t) : Sexp.t =
    List.sexp_of_t Fragment.sexp_of_t (fragments_of path)

  let t_of_sexp (sexp : Sexp.t) : t =
    let frags = List.t_of_sexp Fragment.t_of_sexp sexp in
    Or_error.ok_exn (of_fragments frags)

  let sep (type a) (f : Formatter.t) (a : a) : unit = Fmt.(any "@,.@,") f a

  let pp : t Fmt.t = Fmt.(using fragments_of (list ~sep Fragment.pp))
end

include Complete

module With_meta = struct
  type t = Complete.t Path_meta.With_meta.t [@@deriving sexp, compare, equal]

  let pp : t Fmt.t =
    Fmt.(
      using (Accessor.get Path_meta.With_meta.path) pp
      ++ sp
      ++ using (Accessor.get Path_meta.With_meta.meta) Path_meta.pp)

  let flag (f : Path_meta.Flag.t) :
      ('i, bool, t, [< field]) Accessor.Simple.t =
    Path_meta.(With_meta.meta @> flags @> Accessor.Set.at f)
end
