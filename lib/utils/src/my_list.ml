(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Span = struct
  type t = {pos: int; len: int} [@@deriving sexp]

  let end_pos ({pos; len} : t) : int = pos + len
end

let find_at_most_one (type a b) ?(item_name : string = "item")
    (items : a list) ~(f : a -> b option) ~(on_empty : b Or_error.t) :
    b Or_error.t =
  Or_error.(
    match List.filter_map items ~f with
    | [] ->
        on_empty
    | [x] ->
        return x
    | _ ->
        errorf "Duplicate %s" item_name)

let find_one_opt (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b option Or_error.t =
  let f (x : a) : b option option =
    match f x with Some x -> Some (Some x) | None -> None
  in
  find_at_most_one items ~item_name ~f ~on_empty:(Or_error.return None)

let find_one (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b Or_error.t =
  find_at_most_one items ~item_name ~f
    ~on_empty:(Or_error.errorf "Expected at least one %s" item_name)

let guard_if_empty_opt (xs : 'a list) ~(f : 'a list -> 'b option) : 'b option
    =
  if List.is_empty xs then None else f xs

let guard_if_empty (xs : 'a list) ~(f : 'a list -> 'b) : 'b option =
  guard_if_empty_opt xs ~f:(Fn.compose Option.return f)

module Random = struct
  (* These functions probably raise exceptions on empty lists, so we don't
     expose them directly: see `index` and `span` later on. *)

  let index_raw (xs : 'a list) ~(random : Splittable_random.State.t) : int =
    Splittable_random.int random ~lo:0 ~hi:(List.length xs - 1)

  let span_raw (xs : 'a list) ~(random : Splittable_random.State.t) : Span.t
      =
    let xs_len = List.length xs in
    let pos = Splittable_random.int random ~lo:0 ~hi:(xs_len - 1) in
    let len = Splittable_random.int random ~lo:0 ~hi:(xs_len - pos) in
    {pos; len}

  let index (xs : 'a list) ~(random : Splittable_random.State.t) : int option
      =
    guard_if_empty xs ~f:(index_raw ~random)

  let span (xs : 'a list) ~(random : Splittable_random.State.t) :
      Span.t option =
    guard_if_empty xs ~f:(span_raw ~random)

  let item (xs : 'a list) ~(random : Splittable_random.State.t) : 'a option =
    Option.(index ~random xs >>= List.nth xs)
end

let split_or_error (xs : 'a list) (n : int) : ('a list * 'a list) Or_error.t
    =
  let len = List.length xs in
  if n < 0 then Or_error.errorf "Can't split a list at a negative point %d" n
  else if len < n then
    Or_error.errorf "Can't split a list of length %d at point %d" len n
  else Or_error.return (List.split_n xs n)

(* TODO(@MattWindsor91): if [try_]splice isn't woefully inefficient,
   [try_]map_sub is. Any more efficient implementations gratefully accepted. *)

let try_splice (xs : 'a list) ~span:({pos; len} : Span.t)
    ~(replace_f : 'a list -> 'a list Or_error.t) : 'a list Or_error.t =
  Or_error.Let_syntax.(
    let%bind prefix, rest = split_or_error xs pos in
    let%bind input, suffix = split_or_error rest len in
    let%map output = replace_f input in
    List.concat [prefix; output; suffix])

let splice (xs : 'a list) ~(span : Span.t) ~(replace_f : 'a list -> 'a list)
    : 'a list Or_error.t =
  try_splice xs ~span ~replace_f:(Fn.compose Or_error.return replace_f)

let try_map_sub (xs : 'a list) ~(span : Span.t) ~(f : 'a -> 'a Or_error.t) :
    'a list Or_error.t =
  try_splice xs ~span ~replace_f:(Travesty_base_exts.Or_error.combine_map ~f)

let map_sub (xs : 'a list) ~(span : Span.t) ~(f : 'a -> 'a) :
    'a list Or_error.t =
  splice xs ~span ~replace_f:(List.map ~f)

let eval_guards : (bool * (unit -> 'a)) list -> 'a list =
  List.filter_map ~f:(fun (g, f) -> if g then Some (f ()) else None)

let merge_preserving_order (equal: 'a -> 'a -> bool) (l : 'a list) (r : 'a list) : 'a list =
  (* https://stackoverflow.com/a/36132099 *)
  let (pos, merged) = List.fold l ~init:(0, [])
    ~f:(fun (pos, merged) x ->
      if List.mem merged x ~equal
      then (pos, merged)
      else (
        match List.findi r ~f:(Fn.const (equal x)) with
        | Some (xpos, _) ->
          (xpos + 1, x::(List.rev_append (List.sub r ~pos ~len:(xpos - pos)) merged))
        | None -> (pos, x::merged)
      )
    )
  in
  List.rev_append merged (List.drop r pos)