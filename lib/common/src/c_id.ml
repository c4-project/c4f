(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Tx = Travesty_base_exts
  module Qc = Base_quickcheck
end

let validate_initial_char : char Validate.check =
  Validate.booltest
    Tx.Fn.(Char.is_alpha ||| Char.equal '_')
    ~if_false:"Invalid initial character."

let validate_non_initial_char : char Validate.check =
  Validate.booltest
    Tx.Fn.(Char.is_alphanum ||| Char.equal '_')
    ~if_false:"Invalid character."

let badwords : Set.M(String).t Lazy.t =
  (* `true` and `false` are *not* currently treated as badwords, as
     technically in C they are identifiers and not keywords. This may need to
     be changed, as parts of ACT do treat them as if they were keywords (eg
     in C_mini), but keeping them in `badwords` presently causes large
     amounts of test failures. *)
  lazy (Set.of_list (module String) ["do"; "while"; "if"; "for"; "void"])

let validate_general : string Validate.check = Fn.const Validate.pass

let char_or_underscore (c : char Qc.Generator.t) : char Qc.Generator.t =
  Qc.Generator.(weighted_union [(8.0, c); (1.0, return '_')])

let gen_initial_char : char Qc.Generator.t =
  char_or_underscore Qc.Generator.char_alpha

let gen_non_initial_char : char Qc.Generator.t =
  char_or_underscore Qc.Generator.char_alphanum

module M = Validated.Make_bin_io_compare_hash_sexp (struct
  let here = [%here]

  include String

  let validate_initial_char (c : char) : Validate.t =
    Validate.name
      (Printf.sprintf "initial char '%c'" c)
      (validate_initial_char c)

  let validate_chars (id : string) : Validate.t =
    match String.to_list id with
    | [] ->
        Validate.fail_s [%message "Identifiers can't be empty" ~id]
    | c :: cs ->
        Validate.combine (validate_initial_char c)
          (Validate.list
             ~name:(Printf.sprintf "char '%c'")
             validate_non_initial_char cs)

  let is_badword (s : string) : bool =
    (* Not pointfree because of the need to force a lazy value. *)
    Set.mem (Lazy.force badwords) s

  let validate_badwords : string Validate.check =
    Validate.booltest (Fn.non is_badword)
      ~if_false:"not an allowed identifier"

  let validate : t Validate.check =
    Validate.all [validate_chars; validate_badwords; validate_general]

  let validate_binio_deserialization = true
end)

include M
include Comparable.Make (M)

let to_string : t -> string = raw

let of_string : string -> t = create_exn

let pp : t Fmt.t = Fmt.of_to_string to_string

let is_string_safe (str : string) : bool = Or_error.is_ok (create str)

module Q : Quickcheck.S with type t := t = struct
  let quickcheck_generator : t Qc.Generator.t =
    (* Make a best first attempt to generate valid Herd-safe identifiers, and
       then throw away any that violate more esoteric requirements such as
       no-program-ids. *)
    Qc.Generator.filter_map
      ~f:(Fn.compose Result.ok create)
      (Act_utils.My_quickcheck.gen_string_initial ~initial:gen_initial_char
         ~rest:gen_non_initial_char)

  let quickcheck_observer : t Quickcheck.Observer.t =
    Quickcheck.Observer.unmap String.quickcheck_observer ~f:raw

  let create_opt : string -> t option = Fn.compose Result.ok create

  let quickcheck_shrinker : t Quickcheck.Shrinker.t =
    Quickcheck.Shrinker.create (fun ident ->
        ident |> raw
        |> Quickcheck.Shrinker.shrink String.quickcheck_shrinker
        |> Sequence.filter_map ~f:create_opt)
end

module Json : Plumbing.Jsonable_types.S with type t := t = struct
  let yojson_of_t (id : t) : Yojson.Safe.t = `String (raw id)

  let t_of_yojson (json : Yojson.Safe.t) : t =
    Result.(
      json |> Yojson.Safe.Util.to_string_option
      |> of_option ~error:(Error.of_string "Not a JSON string.")
      >>= create |> Or_error.ok_exn)
end

include Json
include Q

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

module Human = struct
  let adjectives : string list =
    [ "asinine"
    ; "brutal"
    ; "clumsy"
    ; "daring"
    ; "extravagant"
    ; "foolish"
    ; "gentle"
    ; "humble"
    ; "intriguing"
    ; "jocular"
    ; "kind"
    ; "loud"
    ; "modest"
    ; "noble"
    ; "opulent"
    ; "poor"
    ; "quick"
    ; "robust"
    ; "strong"
    ; "tall"
    ; "useless"
    ; "virtuous"
    ; "wise"
    ; "xenophilic"
    ; "yellow"
    ; "zonal" ]

  let nouns : string list =
    [ "aardvark"
    ; "asymptote"
    ; "bandage"
    ; "bridge"
    ; "corridor"
    ; "cucumber"
    ; "dampener"
    ; "door"
    ; "easel"
    ; "entrance"
    ; "fire"
    ; "fox"
    ; "game"
    ; "gopher"
    ; "heap"
    ; "house"
    ; "ibex"
    ; "intern"
    ; "jam"
    ; "juniper"
    ; "kelp"
    ; "knife"
    ; "lane"
    ; "llama"
    ; "map"
    ; "mouse"
    ; "orb"
    ; "phone"
    ; "queue"
    ; "roost"
    ; "staple"
    ; "television"
    ; "toaster"
    ; "unicorn"
    ; "vice"
    ; "volcano"
    ; "warden"
    ; "winter"
    ; "xylophone"
    ; "yam"
    ; "yurt"
    ; "zebra"
    ; "zodiac" ]

  type t = M.t [@@deriving compare, sexp]

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.(
      Let_syntax.(
        let%map adj = option (of_list adjectives)
        and noun = of_list nouns
        and nums = option small_positive_or_zero_int in
        let num = Option.map ~f:Int.to_string nums in
        let str =
          String.concat ~sep:"_" (List.filter_opt [adj; Some noun; num])
        in
        of_string str))

  let quickcheck_shrinker = Q.quickcheck_shrinker

  let quickcheck_observer = Q.quickcheck_observer
end
