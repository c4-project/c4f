(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

type t = Bool of bool | Int of int
[@@deriving compare, equal, sexp, quickcheck, variants]

let truth : t = Bool true

let falsehood : t = Bool false

let type_of : t -> Type.t = function
  | Bool _ ->
      Type.bool ()
  | Int _ ->
      Type.int ()

let reduce (k : t) ~(int : int -> 'a) ~(bool : bool -> 'a) : 'a =
  match k with Bool b -> bool b | Int i -> int i

let as_bool : t -> bool Or_error.t =
  reduce ~bool:Or_error.return ~int:(fun _ ->
      Or_error.error_string "expected bool; got int")

let as_int : t -> int Or_error.t =
  reduce ~int:Or_error.return ~bool:(fun _ ->
      Or_error.error_string "expected int; got bool")

let pp (f : Formatter.t) : t -> unit =
  reduce ~int:(Fmt.int f) ~bool:(Fmt.bool f)

let to_yojson : t -> Yojson.Safe.t = function
  | Bool d ->
      `Bool d
  | Int i ->
      `Int i

let of_yojson (json : Yojson.Safe.t) : (t, string) Result.t =
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

let gen_int32_as_int : int Generator.t =
  Generator.map [%quickcheck.generator: int32] ~f:(fun x ->
      Option.value ~default:0 (Int.of_int32 x))

let gen_int32 : t Generator.t = Generator.map ~f:int gen_int32_as_int
