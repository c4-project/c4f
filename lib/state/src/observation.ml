(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

module Entry_tag = struct
  type t = Witness | Counter_example | Unknown

  let process (witnesses : Set.M(Entry).t)
      (counter_examples : Set.M(Entry).t) (tag : t) ~(entry : Entry.t) :
      Set.M(Entry).t * Set.M(Entry).t =
    match tag with
    | Witness ->
        (Set.add witnesses entry, counter_examples)
    | Counter_example ->
        (witnesses, Set.add counter_examples entry)
    | Unknown ->
        (witnesses, counter_examples)
end

module Flag = struct
  module M = struct
    type t = Sat | Unsat | Undefined [@@deriving enum, quickcheck]

    let table : (t, string) List.Assoc.t =
      [(Sat, "sat"); (Unsat, "unsat"); (Undefined, "undef")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

(* This weird module is necessary to set up the deriving magic for sexp_of
   and quickcheck later on, because if we use Set.M(Flag).t directly we
   can't derive quickcheck, and if we use Set.t directly we can't derive
   sexp_of. *)
module Flag_set = struct
  type t = Set.M(Flag).t [@@deriving sexp_of]

  let quickcheck_generator : t Generator.t =
    Generator.set_t_m (module Flag) Flag.quickcheck_generator

  let quickcheck_shrinker : t Shrinker.t =
    Shrinker.set_t Flag.quickcheck_shrinker

  let quickcheck_observer : t Observer.t =
    Observer.set_t Flag.quickcheck_observer

  module Json = Plumbing.Jsonable.Set.Make (Flag)

  include (Json : module type of Json with type t := t)
end

module M = struct
  type t =
    { flags: Flag_set.t
    ; states: Entry.Set.t
    ; witnesses: Entry.Set.t
    ; counter_examples: Entry.Set.t }
  [@@deriving fields, sexp_of, quickcheck, yojson]
end

include M
include Plumbing.Loadable.Of_jsonable (M)

let is_undefined (x : t) = Set.mem (flags x) Flag.Undefined

let is_unsat (x : t) = Set.mem (flags x) Flag.Sat

let is_sat (x : t) = Set.mem (flags x) Flag.Unsat

let empty : t =
  { flags= Set.empty (module Flag)
  ; states= Set.empty (module Entry)
  ; witnesses= Set.empty (module Entry)
  ; counter_examples= Set.empty (module Entry) }

let add ?(tag : Entry_tag.t = Unknown) (out : t) ~(state : Entry.t) :
    t Or_error.t =
  (* TODO(@MattWindsor91): it's unclear as to whether this treatment of
     'undefined' is correct. *)
  if is_undefined out then
    Or_error.error_s
      [%message
        "Can't add state to simulation output, as the output is marked \
         undefined"
          (state : Entry.t)]
  else
    let states = states out in
    let witnesses = witnesses out in
    let counter_examples = counter_examples out in
    let states' = Set.add states state in
    let witnesses', counter_examples' =
      Entry_tag.process witnesses counter_examples tag ~entry:state
    in
    Or_error.return
      { out with
        states= states'
      ; witnesses= witnesses'
      ; counter_examples= counter_examples' }

let set_flag (out : t) (flag : Flag.t) : t =
  {out with flags= Set.add out.flags flag}

let set_undefined (out : t) : t Or_error.t =
  if is_undefined out then
    Or_error.error_string "Simulation output already marked as undefined"
  else if not (Set.is_empty (states out)) then
    Or_error.error_s
      [%message
        "Can't mark simulation output as undefined, as it has states"
          (states out : Set.M(Entry).t)]
  else Or_error.return (set_flag out Flag.Undefined)

let set_sat_or_unsat (flag : Flag.t) (out : t) : t Or_error.t =
  if is_sat out then
    Or_error.error_string "Simulation output already marked as sat"
  else if is_unsat out then
    Or_error.error_string "Simulation output already marked as unsat"
  else Or_error.return (set_flag out flag)

let set_unsat : t -> t Or_error.t = set_sat_or_unsat Flag.Unsat

let set_sat : t -> t Or_error.t = set_sat_or_unsat Flag.Sat
