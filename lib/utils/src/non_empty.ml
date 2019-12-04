(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* TODO(@MattWindsor91): move this to the next major release of Travesty. *)

(* List.t rather than list so as not to trigger [make]'s normal behaviour of
   translating lists to optional arguments. *)
type 'a t = {head: 'a; tail: 'a List.t} [@@deriving fields, make]

let cons (x : 'a) ({head; tail} : 'a t) : 'a t = {head= x; tail= head :: tail}

let of_list_err : 'a list -> 'a t Or_error.t = function
  | [] ->
      Or_error.error_string "List must be non-empty."
  | head :: tail ->
      Or_error.return (make ~head ~tail)

let of_list_opt (xs : 'a list) : 'a t option =
  xs |> of_list_err |> Or_error.ok

let of_list_exn (xs : 'a list) : 'a t = xs |> of_list_err |> Or_error.ok_exn

let to_list ({head; tail} : 'a t) : 'a list = head :: tail
