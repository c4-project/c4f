(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

open struct
  module A = Accessor_base
end

module Acc = struct
  (* TODO(@MattWindsor91): convert to int32; int is potentially 31 bits wide
     and the generators assume it isn't *)
  type t = Bool of bool | Int of int
  [@@deriving compare, equal, sexp, quickcheck, accessors]

  let true_ = [%accessor A.(bool @> A.Bool.true_)]

  let false_ = [%accessor A.(bool @> A.Bool.false_)]

  let int_zero =
    [%accessor
      A.variant
        ~match_:(function 0 -> First () | n -> Second n)
        ~construct:(fun () -> 0)]

  let zero = [%accessor A.(int @> int_zero)]
end

include Acc
include Comparable.Make (Acc)

let bool = A.construct bool

let int = A.construct int

let truth : t = Bool true

let falsehood : t = Bool false

let prim_type_of : t -> Type.Prim.t = function
  | Bool _ ->
      Bool
  | Int _ ->
      Int

let type_of (x : t) : Type.t = Type.make (Type.Basic.make (prim_type_of x))

let zero_of_type (t : Type.t) : t =
  if
    Type.(
      is_pointer t
      || Prim.eq Accessor.(Access.basic_type @> Basic.Access.prim) t ~to_:Int)
  then Int 0
  else Bool false

let is_bool : t -> bool = function Bool _ -> true | Int _ -> false

let is_int : t -> bool = function Int _ -> true | Bool _ -> false

let reduce (k : t) ~(int : int -> 'a) ~(bool : bool -> 'a) : 'a =
  match k with Bool b -> bool b | Int i -> int i

let as_bool : t -> bool Or_error.t =
  Fn.compose
    (Result.of_option ~error:(Error.of_string "expected bool; got int"))
    (A.get_option Acc.bool)

let as_int : t -> int Or_error.t =
  Fn.compose
    (Result.of_option ~error:(Error.of_string "expected int; got bool"))
    (A.get_option Acc.int)

let pp (f : Formatter.t) : t -> unit =
  reduce ~int:(Fmt.int f) ~bool:(Fmt.bool f)

let yojson_of_t : t -> Yojson.Safe.t = function
  | Bool d ->
      `Bool d
  | Int i ->
      `Int i

let t_of_yojson' (json : Yojson.Safe.t) : (t, string) Result.t =
  let js = [json] in
  Yojson.Safe.Util.(
    match filter_int js with
    | [i] ->
        Result.return (int i)
    | _ -> (
      match filter_bool js with
      | [b] ->
          Result.return (bool b)
      | _ ->
          Result.fail "malformed JSON encoding of C literal" ))

let t_of_yojson (json : Yojson.Safe.t) : t =
  Result.ok_or_failwith (t_of_yojson' json)

let gen_bool : t Generator.t =
  Generator.map ~f:bool [%quickcheck.generator: bool]

let gen_int32_positive_pow2 : int32 Generator.t =
  (* We can't safely generate up to 31 bits here, because that would give us
     INT_MAX+1 on 32-bit-int architectures. *)
  (* We already generate 1, so we don't need it here. *)
  Generator.(map (int32_inclusive 1l 30l) ~f:(Int32.pow 2l))

let gen_int32_negative_pow2 : int32 Generator.t =
  (* This generator doesn't generate min_value; the intermediate calculation
     would overflow if we did it naively, and we generate it in the main
     generator anyway.. *)
  Generator.map ~f:Int32.neg gen_int32_positive_pow2

let gen_int32_positive_pow2min1 : int32 Generator.t =
  (* This generator doesn't generate max_value; the intermediate calculation
     would overflow if we did it naively, and we generate it in the main
     generator anyway.. *)
  Generator.map ~f:Int32.pred gen_int32_positive_pow2

let gen_int32_negative_pow2plus1 : int32 Generator.t =
  Generator.map ~f:Int32.succ gen_int32_negative_pow2

let int32 (k : int32) : t = Int (Int32.to_int_trunc k)

let gen_int32 : t Generator.t =
  (* As usual, we're assuming that ints are 32-bit twos-complement. This is
     slightly naughty. *)
  Generator.map ~f:int32
    Generator.(
      weighted_union
        [ (24.0, int32)
        ; (3.0, return 1l)
        ; (3.0, return 0l)
        ; (2.0, return (-1l))
        ; (2.0, return Int32.max_value)
        ; (2.0, return Int32.min_value)
          (* These don't generate min_value or max_value. *)
        ; (1.0, gen_int32_positive_pow2)
        ; (1.0, gen_int32_positive_pow2min1)
        ; (1.0, gen_int32_negative_pow2)
        ; (1.0, gen_int32_negative_pow2plus1) ])

let quickcheck_generator : t Generator.t =
  Base_quickcheck.Generator.union [gen_int32; gen_bool]

let convert_as_bool : t -> bool Or_error.t =
  (* Draft C11, 6.3.1.2 - sort of. We represent _Bool 0 as false, and _Bool 1
     as true. *)
  function
  | Bool b ->
      Ok b
  | Int 0 ->
      Ok false
  | Int _ ->
      Ok true

let convert_as_int : t -> int Or_error.t = function
  | Int k ->
      Ok k
  | Bool false ->
      Ok 0
  | Bool true ->
      Ok 1

let convert (x : t) ~(to_ : Type.Prim.t) : t Or_error.t =
  (* The Or_error wrapper is future-proofing for if we have unconvertable
     constants later on. *)
  match to_ with
  | Bool ->
      Or_error.(x |> convert_as_bool >>| fun x -> Bool x)
  | Int ->
      Or_error.(x |> convert_as_int >>| fun x -> Int x)
