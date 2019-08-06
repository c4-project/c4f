(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

module M = struct
  type t = {states: Entry.t list; is_undefined: bool}
  [@@deriving fields, sexp_of, quickcheck, yojson]
end

include M
include Plumbing.Loadable.Of_jsonable (M)

(** [init ()] generates an initial [t]. *)
let init () = {states= []; is_undefined= false}

let add (out : t) ~(state : Entry.t) : t Or_error.t =
  match out with
  | {is_undefined= true; _} ->
      Or_error.error_s
        [%message
          "Can't add state to simulation output, as the output is marked \
           undefined"
            (state : Entry.t)]
  | {is_undefined= false; states} ->
      Or_error.return {is_undefined= false; states= state :: states}

let set_undefined : t -> t Or_error.t = function
  | {is_undefined= true; _} ->
      Or_error.error_string "Simulation output already marked as undefined"
  | {states= []; _} ->
      Or_error.return {states= []; is_undefined= true}
  | {states; _} ->
      Or_error.error_s
        [%message
          "Can't mark simulation output as undefined, as it has states"
            (states : Entry.t list)]
