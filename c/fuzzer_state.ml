(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

type t =
  { rng  : Splittable_random.State.t
  ; vars : Fuzzer_var.Map.t
  }
[@@deriving fields]
;;

let init
    (rng : Splittable_random.State.t)
    (globals : Mini.Type.t C_identifier.Map.t)
    (locals  : C_identifier.Set.t)
  : t =
  let vars =
    Fuzzer_var.Map.make_existing_var_map globals locals in
  { rng ; vars }
;;

let map_vars (s : t) ~(f : Fuzzer_var.Map.t -> Fuzzer_var.Map.t) : t =
  { s with vars = f s.vars }
;;

let register_global
    ?(initial_value : Fuzzer_var.Value.t option)
    (s : t)
    (var : C_identifier.t) (ty : Mini.Type.t) : t =
  map_vars s
    ~f:(
      fun v ->
        Fuzzer_var.Map.register_global v ?initial_value var ty
    )
;;

let erase_var_value
    (s : t) ~(var : C_identifier.t) : t =
  map_vars s ~f:(Fuzzer_var.Map.erase_value ~var)

(** [gen_var_raw rng] generates a random C identifier, in
    string format, using [rng] as the RNG. *)
let gen_var_raw (rng : Splittable_random.State.t) : string =
  let module Q = Quickcheck.Generator in
  sprintf "%c%d"
    (Q.generate ~size:0 Q.char_alpha rng)
    (Q.generate ~size:5 Q.small_non_negative_int rng)
;;

let%expect_test "gen_var_raw: example" =
  let deterministic_rng = Splittable_random.State.of_int 0 in
  print_string (gen_var_raw deterministic_rng);
  [%expect {| P0 |}]
;;

(** [gen_var rng] generates a random C identifier, in
    {{!C_identifier.t}C_identifier.t} format, using [rng] as the
    RNG. *)
let gen_var (rng : Splittable_random.State.t) : C_identifier.t =
  (* Assuming that [gen_var_raw] produces valid C identifiers by
     construction. *)
  C_identifier.of_string (gen_var_raw rng)
;;

module Monad = struct
  include Travesty.State_transform.Make (struct
      module Inner = Or_error
      type nonrec t = t
    end)
  ;;

  let with_vars_m (f : Fuzzer_var.Map.t -> 'a t) : 'a t =
    peek vars >>= f
  ;;

  let with_vars (f : Fuzzer_var.Map.t -> 'a) : 'a t =
    peek vars >>| f
  ;;

  let with_rng_m (f : Splittable_random.State.t -> 'a t) : 'a t =
    peek rng >>= f
  ;;

  let with_rng (f : Splittable_random.State.t -> 'a) : 'a t =
    peek rng >>| f
  ;;

  let register_global
      ?(initial_value : Fuzzer_var.Value.t option)
      (ty : Mini.Type.t)
      (var : C_identifier.t)
    : unit t =
    modify (fun s -> register_global ?initial_value s var ty)

  (** [gen_fresh_var ()] is a stateful action that generates a
      variable not already registered in the state (but doesn't
      register it itself). *)
  let gen_fresh_var () : C_identifier.t t =
    let open Let_syntax in
    let%bind rng  = peek rng in
    let%map  vars = peek vars in
    let rec mu () =
      let var = gen_var rng in
      if C_identifier.Map.mem vars var then mu () else var
    in mu ()
  ;;

  (** [gen_and_register_fresh_var ?value ty] is a stateful action that
      generates a variable name not already
      registered in the state, then registers it as a generated
      variable of type [ty] and optional value [value]. *)
  let gen_and_register_fresh_var
      ?(initial_value : Fuzzer_var.Value.t option)
      (ty : Mini.Type.t)
    : C_identifier.t t =
    gen_fresh_var () >>= tee_m ~f:(register_global ?initial_value ty)
  ;;

  let erase_var_value (var : C_identifier.t) : unit t =
    modify (erase_var_value ~var)
end
