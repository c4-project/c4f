(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

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

(* These functions probably raise exceptions on empty lists, so we don't
   expose them directly: see `random_index` and `random_stride` later on. *)

let random_index_raw (xs : 'a list) ~(random : Splittable_random.State.t) :
    int =
  Splittable_random.int random ~lo:0 ~hi:(List.length xs - 1)

let random_stride_raw (xs : 'a list) ~(random : Splittable_random.State.t) :
    int * int =
  let ix1 = random_index_raw xs ~random in
  let ix2 = random_index_raw xs ~random in
  let ixmin = min ix1 ix2 in
  let ixmax = max ix1 ix2 in
  (ixmin, ixmax - ixmin)

let guard_if_empty (xs : 'a list) ~(f : 'a list -> 'b) : 'b option =
  if List.is_empty xs then None else Some (f xs)

let random_index (xs : 'a list) ~(random : Splittable_random.State.t) :
    int option =
  guard_if_empty xs ~f:(random_index_raw ~random)

let random_stride (xs : 'a list) ~(random : Splittable_random.State.t) :
    (int * int) option =
  guard_if_empty xs ~f:(random_stride_raw ~random)

let random_item (xs : 'a list) ~(random : Splittable_random.State.t) :
    'a option =
  Option.(random_index ~random xs >>= List.nth xs)

let split_or_error (xs : 'a list) (n : int) : ('a list * 'a list) Or_error.t
    =
  let len = List.length xs in
  if n < 0 then
    Or_error.errorf "Can't split a list at a negative point %d" n
  else if len < n then
    Or_error.errorf "Can't split a list of length %d at point %d" len n
  else Or_error.return (List.split_n xs n)

let splice (xs : 'a list) ~(start : int) ~(length : int)
    ~(replace_f : 'a list -> 'a list) : 'a list Or_error.t =
  Or_error.Let_syntax.(
    let%bind prefix, rest = split_or_error xs start in
    let%map input, suffix = split_or_error rest length in
    let output = replace_f input in
    List.concat [prefix; output; suffix])
