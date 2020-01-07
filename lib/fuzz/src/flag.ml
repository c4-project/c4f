(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

include Core_kernel

module Raw = struct
  type t = {wins: int; losses: int}
  [@@deriving bin_io, compare, hash, fields, sexp]

  let here = [%here]

  let validate_binio_deserialization = false

  let validate_fields (t : t) : Validate.t =
    let w check = Validate.field_folder t check in
    Validate.of_list
      (Fields.fold ~init:[]
         ~wins:(w Int.validate_non_negative)
         ~losses:(w Int.validate_non_negative))

  let validate_total_positive (t : t) : Validate.t =
    Validate.name "total" (Int.validate_positive (wins t + losses t))

  let validate : t Validate.check =
    Validate.all [validate_fields; validate_total_positive]

  let to_exact_opt : t -> bool option =
    (* Assuming that both odds being 0 is correctly filtered against in the
       validation logic. *)
    function
    | {wins= _; losses= 0} ->
        Some true
    | {wins= 0; losses= _} ->
        Some false
    | {wins= _; losses= _} ->
        None

  let eval_with_random (flag : t) ~(random : Splittable_random.State.t) :
      bool =
    let w = wins flag in
    let l = losses flag in
    let marker = Splittable_random.int random ~lo:0 ~hi:(w + l - 1) in
    marker < w

  let eval (flag : t) ~(random : Splittable_random.State.t) : bool =
    (* No point using the RNG if the outcome is pre-ordained. *)
    match to_exact_opt flag with
    | Some b ->
        b
    | None ->
        eval_with_random flag ~random

  let pp (f : Formatter.t) (flag : t) : unit =
    match to_exact_opt flag with
    | Some b ->
        Bool.pp f b
    | None ->
        Fmt.pf f "%d:%d@ odds on" (wins flag) (losses flag)
end

include Validated.Make_bin_io_compare_hash_sexp (Raw)

let try_make ~(wins : int) ~(losses : int) : t Or_error.t =
  create (Raw.Fields.create ~wins ~losses)

let exact : bool -> t = function
  | true ->
      create_exn {wins= 1; losses= 0}
  | false ->
      create_exn {wins= 0; losses= 1}

let wins : t -> int = Fn.compose Raw.wins raw

let losses : t -> int = Fn.compose Raw.losses raw

let to_exact_opt : t -> bool option = Fn.compose Raw.to_exact_opt raw

let eval (flag : t) ~(random : Splittable_random.State.t) : bool =
  Raw.eval (raw flag) ~random

let as_quickcheck_generator (flag : t) : bool Base_quickcheck.Generator.t =
  Base_quickcheck.Generator.create (fun ~size -> ignore size; eval flag)

let pp : t Fmt.t = Fmt.using raw Raw.pp
