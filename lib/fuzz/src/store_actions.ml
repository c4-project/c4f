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
  module Cm = Act_c_mini
end

module type S =
  Action_types.S with type Payload.t = Cm.Atomic_store.t Payload.Insertion.t

let readme_preamble : string =
  {|
    Generates a store operation on a randomly selected fuzzer-generated
    global variable.
  |}

(** Functor for generating variants of the store action. *)
module Make (B : sig
  val name_suffix : Ac.Id.t
  (** [name_suffix] is the name of the action, less 'store.make'. *)

  val readme_insert : string
  (** [readme_insert] is the part of the action readme specific to this form
      of the store action. *)

  val dst_type : Cm.Type.Basic.t
  (** [dst_type] is the value type of the destination. *)

  val path_filter : Path_filter.t
  (** [path_filter] is the filter to apply on statement insertion paths
      before considering them for the atomic store. *)

  val extra_dst_restrictions : (Var.Record.t -> bool) list
  (** [extra_dst_restrictions] is a list of additional restrictions to place
      on the destination variables (for example, 'must not have
      dependencies'). *)

  module Flags : Storelike_types.Flags

  (** A functor that produces a quickcheck instance for atomic stores given
      source and destination variable environments. *)
  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t := Cm.Atomic_store.t
end) :
  Action_types.S with type Payload.t = Cm.Atomic_store.t Payload.Insertion.t =
Storelike.Make (struct
  let name = Act_common.Id.("store" @: "make" @: B.name_suffix)

  let readme_preamble : string list = [readme_preamble; B.readme_insert]

  include B

  type t = Cm.Atomic_store.t [@@deriving sexp]

  let to_stm : Cm.Atomic_store.t -> Cm.Prim_statement.t =
    Cm.Prim_statement.atomic_store

  let dst_ids (x : Cm.Atomic_store.t) : Ac.C_id.t list =
    [Cm.Address.variable_of (Cm.Atomic_store.dst x)]

  let src_exprs (x : Cm.Atomic_store.t) : Cm.Expression.t list =
    [Cm.Atomic_store.src x]
end)

module Int : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.normal"

  let readme_insert : string =
    "This variant can insert anywhere and target any source and destination."

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Fn.non Var.Record.has_dependencies]

  module Flags = struct
    let erase_known_values = true

    let respect_src_dependencies = true
  end

  let dst_type = Cm.Type.Basic.int ~atomic:true ()

  module Quickcheck = Cm.Atomic_store.Quickcheck_ints
end)

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

  let dst_type = Cm.Type.Basic.int ~atomic:true ()

  module Quickcheck = Cm.Atomic_store.Quickcheck_ints
end)

module Int_redundant : S = Make (struct
  let name_suffix = Ac.Id.of_string "int.redundant"

  let readme_insert : string =
    {| This variant can insert anywhere, but only stores the known value of
       a destination back to itself. |}

  let path_filter = Path_filter.empty

  let extra_dst_restrictions = [Var.Record.has_known_value]

  module Flags = struct
    let erase_known_values = false

    let respect_src_dependencies = true
  end

  let dst_type = Cm.Type.Basic.int ~atomic:true ()

  (* The quickcheck scheme for redundant stores needs to be very different
     from the usual scheme, as it must make sure the source is the
     destination's known value. *)
  module Quickcheck (Src : Cm.Env_types.S) (Dst : Cm.Env_types.S) :
    Act_utils.My_quickcheck.S_with_sexp with type t = Cm.Atomic_store.t =
  struct
    type t = Cm.Atomic_store.t [@@deriving sexp]

    module Q_dst = Cm.Address_gen.Atomic_int_pointers (Dst)

    let quickcheck_observer = Cm.Atomic_store.quickcheck_observer

    (* TODO(@MattWindsor91): allow shrinking the MO? *)
    let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic

    let known_value_expr_of_dest (dst : Q_dst.t) : Cm.Expression.t Or_error.t
        =
      Or_error.Let_syntax.(
        let id = Cm.Address.variable_of dst in
        let%bind kvo = Cm.Env.known_value Dst.env ~id in
        let%map kv =
          Result.of_option kvo
            ~error:(Error.of_string "No known value for this record.")
        in
        Cm.Expression.constant kv)

    let quickcheck_generator =
      (* Deliberately ignore the source environment. TODO(@MattWindsor91):
         optimise this? *)
      Base_quickcheck.Generator.map2 [%quickcheck.generator: Q_dst.t]
        Cm.Mem_order.gen_store ~f:(fun dst mo ->
          (* We're really hoping that the availability check over known
             values works here, because there's no way we can safely return
             an error if not. *)
          let src = Or_error.ok_exn (known_value_expr_of_dest dst) in
          Cm.Atomic_store.make ~src ~dst ~mo)
  end
end)
