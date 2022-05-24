(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Prim = struct
  type t = Bool | Int [@@deriving variants, equal, enumerate]

  let eq (type x) (acc : (unit, t, x, [> Accessor.getter]) Accessor.t)
      (x : x) ~(to_ : t) : bool =
    x |> Accessor.get acc |> equal to_
end

let to_non_atomic (acc : (_, bool, 't, [< Accessor.field]) Accessor.t)
    : 't -> 't Or_error.t =
  Accessor_base.Or_error.map acc ~f:(function
    | true -> Ok false
    | false -> Or_error.error_string "already non-atomic" )

module Basic = struct
  module Access = struct
    type t = {is_atomic: bool [@default false]; prim: Prim.t [@main]}
    [@@deriving accessors, equal, make, enumerate]

    let int ?(is_atomic : bool option) () : t = make Int ?is_atomic

    let bool ?(is_atomic : bool option) () : t = make Bool ?is_atomic

    let table : (t, string) List.Assoc.t =
      [ (int (), "int")
      ; (int ~is_atomic:true (), "atomic_int")
      ; (bool (), "bool")
      ; (bool ~is_atomic:true (), "atomic_bool") ]
  end

  module M_enum = struct
    include Access
    include Utils.Enum.Make_from_enumerate (Access)
  end

  include Access
  include Utils.Enum.Extend_table (M_enum)

  let eq (type x) (acc : (unit, t, x, [> Accessor.getter]) Accessor.t)
      (x : x) ~(to_ : t) : bool =
    x |> Accessor.get acc |> equal to_

  let is_atomic = Accessor.get Access.is_atomic

  let prim = Accessor.get Access.prim

  (* TODO(@MattWindsor91): replace these with the accessor. *)
  let as_atomic : t -> t = Accessor.set Access.is_atomic ~to_:true

  let strip_atomic : t -> t = Accessor.set Access.is_atomic ~to_:false

  let to_non_atomic : t -> t Or_error.t = to_non_atomic Access.is_atomic
end

module Access = struct
  open Base_quickcheck

  type t =
    { basic_type: Basic.t [@main]
    ; is_pointer: bool [@default false]
    ; is_volatile: bool [@default false] }
  [@@deriving make, accessors, equal, compare, quickcheck]

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
         [ Option.some_if (Accessor.get is_volatile ty) volatile_frag
         ; Some (Basic.to_string (Accessor.get basic_type ty))
         ; Option.some_if (Accessor.get is_pointer ty) pointer_frag ] )
end

module M = struct
  include Access
  include Sexpable.Of_stringable (Access)
end

include M

let basic_type = Accessor.get Access.basic_type

let is_volatile = Accessor.get Access.is_volatile

let is_pointer = Accessor.get Access.is_pointer

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

(* TODO(@MattWindsor91): phase the below out and just export the atomicity
   lens *)
let atomicity = Accessor.(Access.basic_type @> Basic.Access.is_atomic)

let is_atomic : t -> bool = Accessor.get atomicity

let strip_atomic : t -> t = Accessor.set atomicity ~to_:false

let as_atomic : t -> t = Accessor.set atomicity ~to_:true

let as_volatile (t : t) : t = {t with is_volatile= true}

let to_non_atomic : t -> t Or_error.t = to_non_atomic atomicity

let bool ?(is_atomic : bool option) ?(is_pointer : bool option)
    ?(is_volatile : bool option) () : t =
  make (Basic.bool ?is_atomic ()) ?is_pointer ?is_volatile

let int ?(is_atomic : bool option) ?(is_pointer : bool option)
    ?(is_volatile : bool option) () : t =
  make (Basic.int ?is_atomic ()) ?is_pointer ?is_volatile

module Json : Plumbing.Jsonable_types.S with type t := t =
  Plumbing.Jsonable.Of_stringable (Access)

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
