(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {wins: int; losses: int} [@@deriving equal, fields, sexp]

let validate_fields (t : t) : Validate.t =
  let w check = Validate.field_folder check t in
  Validate.of_list
    (Fields.fold ~init:[]
       ~wins:(w Core.Int.validate_non_negative)
       ~losses:(w Core.Int.validate_non_negative) )

let validate_total_positive (t : t) : Validate.t =
  Validate.name "total" (Core.Int.validate_positive (wins t + losses t))

let validate : t Validate.check =
  Validate.all [validate_fields; validate_total_positive]

let to_exact_opt : t -> bool option =
  (* Assuming that both odds being 0 is correctly filtered against in the
     validation logic. *)
  function
  | {wins= _; losses= 0} -> Some true
  | {wins= 0; losses= _} -> Some false
  | {wins= _; losses= _} -> None

let eval_with_random (flag : t) ~(random : Splittable_random.State.t) : bool
    =
  let w = wins flag in
  let l = losses flag in
  let marker = Splittable_random.int random ~lo:0 ~hi:(w + l - 1) in
  marker < w

let eval (flag : t) ~(random : Splittable_random.State.t) : bool =
  (* No point using the RNG if the outcome is pre-ordained. *)
  match to_exact_opt flag with
  | Some b -> b
  | None -> eval_with_random flag ~random

let pp (f : Formatter.t) (flag : t) : unit =
  match to_exact_opt flag with
  | Some b -> Bool.pp f b
  | None -> Fmt.pf f "%d:%d@ odds on" (wins flag) (losses flag)

let try_make ~(wins : int) ~(losses : int) : t Or_error.t =
  Validate.valid_or_error validate {wins; losses}

let exact : bool -> t = function
  | true -> {wins= 1; losses= 0}
  | false -> {wins= 0; losses= 1}

let as_quickcheck_generator (flag : t) : bool Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.create (fun ~size -> ignore size ; eval flag)
