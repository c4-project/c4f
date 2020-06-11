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
  module Cm = Act_fir
end

module type S =
  Action_types.S
    with type Payload.t =
          Act_fir.Expression.t Act_fir.Atomic_fetch.t Payload.Insertion.t

let readme_preamble : string =
  {|
    Generates a fetch operation on a randomly selected fuzzer-generated
    global variable.
  |}

(** Functor for generating variants of the fetch action. *)
module Make (B : sig
  val name_suffix : Ac.Id.t
  (** [name_suffix] is the name of the action, less 'fetch.make'. *)

  val readme_insert : string
  (** [readme_insert] is the part of the action readme specific to this form
      of the fetch action. *)

  val dst_type : Cm.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic fetch. *)

  val extra_dst_restrictions : (Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  (** A functor that produces a quickcheck instance for atomic fetchs given
      source and destination variable environments. *)
  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t := Cm.Expression.t Cm.Atomic_fetch.t
end) :
  Action_types.S
    with type Payload.t =
          Cm.Expression.t Cm.Atomic_fetch.t Payload.Insertion.t =
Storelike.Make (struct
  let name = Act_common.Id.("fetch" @: "make" @: B.name_suffix)

  let readme_preamble : string list = [readme_preamble; B.readme_insert]

  include B

  type t = Cm.Expression.t Cm.Atomic_fetch.t [@@deriving sexp]

  let gen ~(src : Cm.Env.t) ~(dst : Cm.Env.t) ~(vars : Var.Map.t)
      ~(tid : int) : t Base_quickcheck.Generator.t =
    let module Src = struct
      let env = src
    end in
    let module Dst = struct
      let env = dst
    end in
    ignore vars ;
    ignore tid ;
    let module Gen = B.Quickcheck (Src) (Dst) in
    [%quickcheck.generator: Gen.t]

  let to_stms (x : Cm.Expression.t Cm.Atomic_fetch.t) :
      Cm.Prim_statement.t list =
    [Cm.Prim_statement.atomic_fetch x]

  let new_locals (_ : Cm.Expression.t Cm.Atomic_fetch.t) :
      Cm.Initialiser.t Ac.C_named.Alist.t =
    []

  let dst_ids (x : Cm.Expression.t Cm.Atomic_fetch.t) : Ac.C_id.t list =
    [Cm.Address.variable_of (Cm.Atomic_fetch.obj x)]

  let src_exprs (x : Cm.Expression.t Cm.Atomic_fetch.t) :
      Cm.Expression.t list =
    [Cm.Atomic_fetch.arg x]
end)

(* TODO(@MattWindsor91): 'Int' module, which would need some care in order to
   be overflow-safe. *)

module Int_dead : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.dead"

  let readme_insert : string =
    {| This variant can target any source and destination, but only inserts
       into dead code.  As it only targets dead code, it does not add
       dependences or erase known-values. |}

  let path_filter = Path_filter.(empty |> in_dead_code_only)

  let extra_dst_restrictions = []

  module Flags = struct
    let erase_known_values = false

    let respect_src_dependencies = false
  end

  let dst_type = Cm.Type.Basic.int ~is_atomic:true ()

  module Quickcheck = Cm.Expression_gen.Atomic_fetch_int_values
end)

module Int_redundant : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.redundant"

  let readme_insert : string =
    {| This variant can insert anywhere, but only fetches the known value of
       a destination back to itself. |}

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Var.Record.has_known_value]

  module Flags = struct
    let erase_known_values = false

    let respect_src_dependencies = true
  end

  let dst_type = Cm.Type.Basic.int ~is_atomic:true ()

  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp
      with type t = Cm.Expression.t Cm.Atomic_fetch.t =
    Cm.Expression_gen.Atomic_fetch_int_nops (Dst) (Src)
end)
