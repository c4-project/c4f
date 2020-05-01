(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Au = Act_utils
end

module Prim = struct
  type t = Bool | Int [@@deriving variants, equal, enumerate]
end

module Basic = struct
  module M = struct
    type t = {is_atomic: bool; prim: Prim.t}
    [@@deriving equal, fields, enumerate]

    let int ?(is_atomic : bool = false) () : t = {is_atomic; prim= Int}

    let bool ?(is_atomic : bool = false) () : t = {is_atomic; prim= Bool}

    let table : (t, string) List.Assoc.t =
      [ (int (), "int")
      ; (int ~is_atomic:true (), "atomic_int")
      ; (bool (), "bool")
      ; (bool ~is_atomic:true (), "atomic_bool") ]
  end

  module M_enum = struct
    include M
    include Au.Enum.Make_from_enumerate (M)
  end

  include M
  include Au.Enum.Extend_table (M_enum)

  let as_atomic (t : t) : t = {t with is_atomic= true}

  let strip_atomic (t : t) : t = {t with is_atomic= false}

  let to_non_atomic : t -> t Or_error.t = function
    | {is_atomic= true; prim} ->
        Or_error.return {is_atomic= false; prim}
    | _ ->
        Or_error.error_string "already non-atomic"
end

module M1 = struct
  open Base_quickcheck

  type t =
    { basic_type: Basic.t [@main]
    ; is_pointer: bool [@default false]
    ; is_volatile: bool [@default false] }
  [@@deriving make, fields, equal, compare, quickcheck]

  let volatile_frag : string = "volatile "

  let pointer_frag : string = "*"

  let scrape (s : string) ~(f : string -> string option) : string * bool =
    match f s with None -> (s, false) | Some s' -> (String.strip s', true)

  let scrape_volatile : string -> string * bool =
    scrape ~f:(String.chop_prefix ~prefix:volatile_frag)

  let scrape_pointer : string -> string * bool =
    scrape ~f:(String.chop_suffix ~suffix:pointer_frag)

  let of_string (s : string) : t =
    let s', is_volatile = scrape_volatile s in
    let s'', is_pointer = scrape_pointer s' in
    let basic_type = Basic.of_string s'' in
    make basic_type ~is_pointer ~is_volatile

  let to_string (ty : t) : string =
    String.concat
      (List.filter_opt
         [ Option.some_if (is_volatile ty) volatile_frag
         ; Some (Basic.to_string (basic_type ty))
         ; Option.some_if (is_pointer ty) pointer_frag ])
end

module M = struct
  include M1
  include Sexpable.Of_stringable (M1)
end

include M

let basic_type_is (ty : t) ~(basic : Basic.t) : bool =
  Basic.equal (basic_type ty) basic

let deref (ty : t) : t Or_error.t =
  if ty.is_pointer then Ok {ty with is_pointer= false}
  else
    Or_error.error_s
      [%message
        "tried to get value type of a non-pointer type"
          (ty.basic_type : Basic.t)]

let ref (ty : t) : t Or_error.t =
  if not ty.is_pointer then Ok {ty with is_pointer= true}
  else Or_error.error_string "already a pointer type"

let is_atomic (ty : t) : bool = Basic.is_atomic (basic_type ty)

(* for now *)

let strip_atomic (ty : t) : t =
  {ty with basic_type= Basic.strip_atomic ty.basic_type}

let as_atomic (ty : t) : t =
  {ty with basic_type= Basic.as_atomic ty.basic_type}

let to_non_atomic (ty : t) : t Or_error.t =
  Or_error.(
    ty |> basic_type |> Basic.to_non_atomic
    >>| fun b -> {ty with basic_type= b})

let bool ?(is_atomic : bool option) ?(is_pointer : bool option)
    ?(is_volatile : bool option) () : t =
  make (Basic.bool ?is_atomic ()) ?is_pointer ?is_volatile

let int ?(is_atomic : bool option) ?(is_pointer : bool option)
    ?(is_volatile : bool option) () : t =
  make (Basic.int ?is_atomic ()) ?is_pointer ?is_volatile

module Json : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Of_stringable (M1)

include Json

let check (t1 : t) (t2 : t) : t Or_error.t =
  if equal t1 t2 then Ok t1
  else Or_error.error_s [%message "Type mismatch" ~t1:(t1 : t) ~t2:(t2 : t)]

let check_modulo_atomicity (t1 : t) (t2 : t) : t Or_error.t =
  check (strip_atomic t1) (strip_atomic t2)

let check_atomic_non ~(atomic : t) ~(non : t) : t Or_error.t =
  Or_error.Let_syntax.(
    let%bind natomic = to_non_atomic atomic in
    check natomic non)

let check_pointer_non ~(pointer : t) ~(non : t) : t Or_error.t =
  Or_error.Let_syntax.(
    let%bind npointer = deref pointer in
    check npointer non)
