(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Tx = Travesty_base_exts
open Act_utils

module type Basic = sig
  val here : Lexing.position

  val validate_initial_char : char Validate.check

  val validate_char : char Validate.check
end

module Make (B : Basic) = struct
  include String

  let here = B.here

  let validate_sep : (char * char list) Validate.check =
    Validate.pair
      ~fst:(fun c ->
        Validate.name
          (Printf.sprintf "char '%c'" c)
          (B.validate_initial_char c))
      ~snd:
        (Validate.list ~name:(Printf.sprintf "char '%c'") B.validate_char)

  let validate : t Validate.check =
   fun id ->
    match String.to_list id with
    | [] ->
        Validate.fail_s [%message "Identifiers can't be empty" ~id]
    | c :: cs ->
        validate_sep (c, cs)

  let validate_binio_deserialization = true
end

module M = Validated.Make_bin_io_compare_hash_sexp (Make (struct
  let here = [%here]

  let validate_initial_char : char Validate.check =
    Validate.booltest
      Tx.Fn.(Char.is_alpha ||| Char.equal '_')
      ~if_false:"Invalid initial character."

  let validate_char : char Validate.check =
    Validate.booltest
      Tx.Fn.(Char.is_alphanum ||| Char.equal '_')
      ~if_false:"Invalid character."
end))

include M
include Comparable.Make (M)

let to_string : t -> string = raw

let of_string : string -> t = create_exn

let pp : t Fmt.t = Fmt.of_to_string to_string

let is_string_safe (str : string) : bool = Or_error.is_ok (create str)

module Json : Plumbing.Jsonable_types.S with type t := t = struct
  let yojson_of_t (id : t) : Yojson.Safe.t = `String (raw id)

  let t_of_yojson' (json : Yojson.Safe.t) : (t, string) Result.t =
    Result.(
      json |> Yojson.Safe.Util.to_string_option
      |> of_option ~error:(Error.of_string "Not a JSON string.")
      >>= create
      |> Result.map_error ~f:Error.to_string_hum)

  let t_of_yojson (json : Yojson.Safe.t) : t =
    Result.(
      json |> Yojson.Safe.Util.to_string_option
      |> of_option ~error:(Error.of_string "Not a JSON string.")
      >>= create |> Or_error.ok_exn)
end

include Json

module Q : Quickcheck.S with type t := t = struct
  let char_or_underscore (c : char Quickcheck.Generator.t) :
      char Quickcheck.Generator.t =
    Quickcheck.Generator.(weighted_union [(8.0, c); (1.0, return '_')])

  let c_keywords : Base.Set.M(String).t Lazy.t =
    lazy
      (Base.Set.of_list
         (module String)
         ["do"; "while"; "if"; "for"; "true"; "false"; "void"])

  let is_c_keyword (s : string) : bool =
    (* Not pointfree because of the need to force a lazy value. *)
    Base.Set.mem (Lazy.force c_keywords) s

  let gen_including_c_keywords : t Quickcheck.Generator.t =
    Quickcheck.Generator.map
      (My_quickcheck.gen_string_initial
         ~initial:(char_or_underscore Char.gen_alpha)
         ~rest:(char_or_underscore Char.gen_alphanum))
      ~f:create_exn

  let quickcheck_generator : t Quickcheck.Generator.t =
    Quickcheck.Generator.filter gen_including_c_keywords ~f:(fun id ->
        not (is_c_keyword (raw id)))

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

  let create_opt : string -> t option = Fn.compose Result.ok create

  let inflate_shrunk (s : string) : t option =
    Option.(s |> some_if (not (is_c_keyword s)) >>= create_opt)

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.create (fun ident ->
        ident |> raw
        |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
        |> Sequence.filter_map ~f:inflate_shrunk)
end

include Q

module Herd_safe = struct
  type c = t

  module M = Validated.Make_bin_io_compare_hash_sexp (Make (struct
    let here = [%here]

    let validate_initial_char : char Validate.check =
      Validate.booltest Char.is_alpha
        ~if_false:"Invalid initial character (must be alphabetic)."

    let validate_char : char Validate.check =
      Validate.booltest Char.is_alphanum
        ~if_false:"Invalid non-initial character (must be alphanumeric)."
  end))

  include M
  include Comparable.Make (M)

  let of_c_identifier (cid : c) : t Or_error.t = cid |> to_string |> create

  let to_c_identifier (hid : t) : c = hid |> raw |> of_string

  let is_string_safe (str : string) : bool = Or_error.is_ok (create str)

  let to_string : t -> string = raw

  let of_string : string -> t = create_exn

  let pp : t Fmt.t = Fmt.of_to_string to_string

  module Q : Quickcheck.S with type t := t = struct
    let quickcheck_generator : t Quickcheck.Generator.t =
      Quickcheck.Generator.map
        (My_quickcheck.gen_string_initial ~initial:Char.gen_alpha
           ~rest:Char.gen_alphanum)
        ~f:create_exn

    let quickcheck_observer : t Quickcheck.Observer.t =
      Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

    let quickcheck_shrinker : t Quickcheck.Shrinker.t =
      Quickcheck.Shrinker.create (fun ident ->
          ident |> raw
          |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
          |> Sequence.filter_map ~f:(Fn.compose Result.ok create))
  end

  include Q
end

module Alist = struct
  include Travesty.Bi_traversable.Fix2_left (Travesty_base_exts.Alist) (M)

  (* Value restriction strikes again. *)

  let yojson_of_t (type r) (rhs : r -> Yojson.Safe.t) : r t -> Yojson.Safe.t
      =
    Plumbing.Jsonable.Alist.yojson_of_alist to_string rhs

  let t_of_yojson (type r) (rhs : Yojson.Safe.t -> r) : Yojson.Safe.t -> r t
      =
    Plumbing.Jsonable.Alist.alist_of_yojson of_string rhs

  let t_of_yojson' (type r) (rhs : Yojson.Safe.t -> (r, string) Result.t) :
      Yojson.Safe.t -> (r t, string) Result.t =
    Plumbing.Jsonable.Alist.alist_of_yojson' of_string rhs
end
